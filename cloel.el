;;; cloel.el --- Cloel   -*- lexical-binding: t; -*-

;; Filename: cloel.el
;; Description: Cloel
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2024, Andy Stewart, all rights reserved.
;; Created: 2024-09-19 23:08:26
;; Version: 0.1
;; Last-Updated: 2024-09-19 23:08:26
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/cloel
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Cloel
;;

;;; Installation:
;;
;; Put cloel.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'cloel)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET cloel RET
;;

;;; Change log:
;;
;; 2024/09/19
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'cl-lib)
(require 'parseedn)

;;; Code:
(defvar cloel-apps (make-hash-table :test 'equal)
  "Hash table to store app-specific data.")

(defvar cloel-max-retries 5
  "Maximum number of connection retries.")

(defvar cloel-retry-delay 1
  "Delay in seconds between connection retries.")

(defvar cloel-sync-call-results (make-hash-table :test 'equal)
  "Hash table to store sync call results.")


(defgroup cloel nil
  "Clojure Bridge for Emacs."
  :group 'applications)

(defcustom cloel-max-retries 5
  "Maximum number of connection retries."
  :type 'integer
  :group 'cloel)

(defcustom cloel-retry-delay 1
  "Delay in seconds between connection retries."
  :type 'number
  :group 'cloel)

(defcustom cloel-sync-timeout 60
  "Timeout in seconds for synchronous calls."
  :type 'number
  :group 'cloel)

(defcustom cloel-default-port-file ".cloel-port"
  "Default filename for port discovery."
  :type 'string
  :group 'cloel)


(defun cloel-log-error (format-string &rest args)
  "Log an error message using FORMAT-STRING and ARGS."
  (let ((message (apply #'format format-string args)))
    (message "Cloel Error: %s" message)
    message))


(defun cloel-ensure-process-killed (process)
  "Ensure PROCESS is killed safely."
  (when (and process (process-live-p process))
    (let ((pid (process-id process)))
      (delete-process process)
      (when pid
        (ignore-errors
          (signal-process pid 'SIGTERM)
          (sleep-for 0.5)
          (when (process-running-p pid)
            (signal-process pid 'SIGKILL)))))))


(defun cloel-generate-app-functions (app-name)
  "Generate app-specific functions for APP-NAME."
  (let ((start-func-name (intern (format "cloel-%s-start-process" app-name)))
        (stop-func-name (intern (format "cloel-%s-stop-process" app-name)))
        (restart-func-name (intern (format "cloel-%s-restart-process" app-name)))
        (call-async-func-name (intern (format "cloel-%s-call-async" app-name)))
        (call-sync-func-name (intern (format "cloel-%s-call-sync" app-name)))
        (send-message-func-name (intern (format "cloel-%s-send-message" app-name))))

    (fset start-func-name
          `(lambda ()
             (interactive)
             (cloel-start-process ',app-name)))

    (fset stop-func-name
          `(lambda ()
             (interactive)
             (cloel-stop-process ',app-name)))

    (fset restart-func-name
          `(lambda ()
             (interactive)
             (cloel-restart-process ',app-name)))

    (fset call-async-func-name
          `(lambda (func &rest args)
             (apply 'cloel-call-async ',app-name func args)))

    (fset call-sync-func-name
          `(lambda (func &rest args)
             (apply 'cloel-call-sync ',app-name func args)))

    (fset send-message-func-name
          `(lambda (message)
             (cloel-send-message ',app-name message)))

    (list start-func-name stop-func-name restart-func-name call-async-func-name call-sync-func-name send-message-func-name)))

(defun cloel-register-app (app-name app-dir &optional aliases-or-bb-task clj-type)
  "Register an app with APP-NAME and APP-DIR."
  (let* ((app-dir (expand-file-name app-dir))
         (inferred-clj-type (or clj-type (cloel-determine-app-type app-dir))))
    (unless inferred-clj-type
      (error "Cannot determine app type for directory: %s. Please provide clj-type or ensure deps.edn/bb.edn exists." app-dir))
    (puthash app-name
             (list :dir app-dir
                   :type inferred-clj-type
                   :port-from-file (cloel-get-free-port-from-port-file)
                   :aliases-or-bb-task (or aliases-or-bb-task 'clojure)
                   :server-process nil
                   :tcp-channel nil)
             cloel-apps)
    (cloel-generate-app-functions app-name)))

(defun cloel-get-app-data (app-name)
  "Get the data for APP-NAME."
  (gethash app-name cloel-apps))

(defun cloel-set-app-data (app-name key value)
  "Set KEY to VALUE for APP-NAME."
  (when-let ((app-data (cloel-get-app-data app-name)))
    (puthash app-name (plist-put app-data key value) cloel-apps)))

(defun cloel-get-free-port-from-port-file ()
  "Get port specified in .cloel-port file."
  (let ((port-file (expand-file-name ".cloel-port" default-directory)))
    (when (file-exists-p port-file)
      (with-temp-buffer
        (insert-file-contents port-file)
        (string-to-number (buffer-string))))))

;; (defun cloel-get-free-port ()
;;  "Find a free port."
;;  (let ((process (make-network-process :name "cloel-port-finder"
;;                                       :service t
;;                                       :host 'local
;;                                       :server t)))
;;    (prog1 (process-contact process :service)
;;      (delete-process process))))

(defun cloel-get-free-port ()
  "Find a free port by binding to it and ensuring it remains available."
  (let (port process)
    (while (not port)
      ;; try to bind random port number
      (setq process (make-network-process :name "cloel-port-finder"
                                          :service t
                                          :host 'local
                                          :server t))
      (setq port (process-contact process :service))
      ;; check if port available
      (condition-case nil
          (progn
            (delete-process process)
            (setq process (make-network-process :name "cloel-port-checker"
                                                :service port
                                                :host 'local
                                                :server t))
            (delete-process process)
            (message "Found free port: %d" port))
        (error
         (setq port nil)  ;; if port not useful, try another port
         (when process (delete-process process)))))
    port))

(defun cloel-determine-app-type (app-dir)
  "Determine the type of the Clojure app based on its directory structure."
  (cond
   ((file-exists-p (expand-file-name "deps.edn" app-dir)) 'deps)
   ((file-exists-p (expand-file-name "bb.edn" app-dir)) 'bb)
   ((and (file-directory-p app-dir) (directory-files app-dir t ".*\.clj")) 'clojure-directory)
   ((and (file-regular-p app-dir) (string-suffix-p ".clj" app-dir)) 'clojure)
   (t (error "Cannot determine app type for directory: %s" app-dir))))


(defun cloel-find-main-file (app-name app-dir)
  "Find the main Clojure file for APP-NAME in APP-DIR or its src subdirectory."
  (let ((src-dir (expand-file-name "src" app-dir)))
    (cond
     ;; First look for a file named after the app in src/
     ((and (file-directory-p src-dir)
           (file-exists-p (expand-file-name (format "%s.clj" app-name) src-dir)))
      (expand-file-name (format "%s.clj" app-name) src-dir))

     ;; Then look for any .clj file in src/
     ((and (file-directory-p src-dir)
           (directory-files src-dir t "\\.clj$"))
      (car (directory-files src-dir t "\\.clj$")))

     ;; Look directly in app-dir for app-name.clj
     ((file-exists-p (expand-file-name (format "%s.clj" app-name) app-dir))
      (expand-file-name (format "%s.clj" app-name) app-dir))

     ;; Finally, look for any .clj file in app-dir
     ((directory-files app-dir t "\\.clj$")
      (car (directory-files app-dir t "\\.clj$")))

     ;; Nothing found
     (t (error "Cannot find .clj file for app: %s in directory: %s or src/" app-name app-dir)))))


(defun cloel-start-process (app-name)
  "Start the Clojure server process for APP-NAME."
  (let* ((app-data (cloel-get-app-data app-name))
         (port (plist-get app-data :port-from-file))
         (clj-type (plist-get app-data :type)))
    (if port
        (cloel-connect-with-retry app-name "localhost" port)
      (let* ((app-dir (plist-get app-data :dir))
             (app-aliases (or (plist-get app-data :aliases-or-bb-task) "cloel"))
             (main-file (cloel-find-main-file app-name app-dir))
             (default-directory app-dir)
             (app-deps-edn (expand-file-name "deps.edn" app-dir))
             (app-bb-edn (expand-file-name "bb.edn" app-dir))
             (port (cloel-get-free-port)))
        (when (and (eq clj-type 'clojure) (not (file-exists-p app-deps-edn)))
          (error "can not find app deps.edn at %s" app-deps-edn))
        (when (and (eq clj-type 'deps) (not (file-exists-p app-deps-edn)))
          (error "can not find app deps.edn at %s" app-deps-edn))
        (when (and (eq clj-type 'bb) (not (file-exists-p app-bb-edn)))
          (error "can not find app bb.edn at %s" app-bb-edn))
        (let ((process (start-process (format "cloel-%s-clojure-server" app-name)
                                      (format "*cloel-%s-clojure-server*" app-name)
                                      "sh"
                                      "-c"
                                      (pcase clj-type
                                        ('clojure (format "clojure -M:%s %s %d" app-aliases main-file port))
                                        ('deps (format "clojure -M:%s %s %d" app-aliases main-file port))
                                        ('bb (format "bb %s %d" app-aliases port))
                                        (_ (error "Unknown clj-type: %s" clj-type))))))
          (message "Starting Clojure server with command: %s"
                   (pcase clj-type
                     ('clojure (format "clojure -M:%s %s %d" app-aliases main-file port))
                     ('deps (format "clojure -M:%s %s %d" app-aliases main-file port))
                     ('bb (format "bb %s %d" app-aliases port))))
          (cloel-set-app-data app-name :server-process process)
          (message "Starting Clojure server for %s on port %d" app-name port)
          (set-process-sentinel process
                                (lambda (proc event)
                                  (message "clojure server process for %s has stopped: %s" app-name event)
                                  (cloel-server-process-sentinel proc event app-name)))
          (sleep-for 3)  ;; sleep 3s for some scenes
          (cloel-connect-with-retry app-name "localhost" port))))))

(defun cloel-stop-process (app-name)
  "Stop the Clojure server process and disconnect the client for APP-NAME."
  (let* ((app-data (cloel-get-app-data app-name))
         (tcp-channel (plist-get app-data :tcp-channel))
         (server-process (plist-get app-data :server-process)))
    (when (and tcp-channel (process-live-p tcp-channel))
      (delete-process tcp-channel)
      (cloel-set-app-data app-name :tcp-channel nil)
      (message "Disconnected Clojure TCP connection for %s" app-name))
    (when (and server-process (process-live-p server-process))
      (delete-process server-process)
      (cloel-set-app-data app-name :server-process nil)
      (message "Stopped Clojure server process for %s" app-name))))

(defun cloel-restart-process (app-name)
  "Restart the Clojure server process for APP-NAME."
  (cloel-stop-process app-name)
  (sleep-for 1)
  (cloel-start-process app-name))

(defun cloel-server-process-sentinel (process event app-name)
  "Handle Clojure process state changes for APP-NAME."
  (when (memq (process-status process) '(exit signal))
    (message "Clojure server process for %s has stopped: %s" app-name event)
    (cloel-set-app-data app-name :server-process nil)))

(defun cloel-connect-with-retry (app-name host port)
  "Attempt to connect to the Clojure server with retries for APP-NAME."
  (let ((retries 0)
        (connected nil))
    (while (and (not connected) (< retries cloel-max-retries))
      (condition-case err
          (progn
            (cloel-connect app-name host port)
            (setq connected t))
        (error
         (setq retries (1+ retries))
         (when (< retries cloel-max-retries)
           (sleep-for cloel-retry-delay)))))
    (unless connected
      (error "Failed to connect after %d attempts for %s" cloel-max-retries app-name))))

(defun cloel-connect (app-name host port)
  "Establish a connection to a Clojure server at HOST:PORT for APP-NAME."
  (let* ((port-num (if (stringp port) (string-to-number port) port))
         (channel (open-network-stream (format "cloel-%s-client" app-name)
                                       (format "*cloel-%s-client*" app-name)
                                       host port-num)))
    (cloel-set-app-data app-name :tcp-channel channel)

    ;; TODO: this is ugly
    ;; use channel as server-process when port file exists
    ;; so that process-live-p against :server-process works
    (let ((app-data (cloel-get-app-data app-name)))
      (when (and (plist-get app-data :port-from-file)
                 (not (plist-get app-data :server-process)))
        (cloel-set-app-data app-name :server-process channel)))

    (set-process-coding-system channel 'utf-8 'utf-8)
    (if (process-live-p channel)
        (progn
          (set-process-filter channel
                              (lambda (proc output)
                                (cloel-tcp-connection-filter proc output app-name)))
          (set-process-sentinel channel
                                (lambda (proc event)
                                  (cloel-tcp-connection-sentinel proc event app-name)))
          (message "TCP connected for %s at %s:%s" app-name host port-num))
      (error "Failed to TCP connect for %s at %s:%s" app-name host port-num))))

(defun cloel-send-message (app-name message)
  "Send MESSAGE to the connected Clojure server for APP-NAME."
  (let ((channel (plist-get (cloel-get-app-data app-name) :tcp-channel)))
    (if (process-live-p channel)
        (let ((encoded-message
               (condition-case err
                   (parseedn-print-str message)
                 (error
                  (message "Error encoding message: %S" err)
                  (prin1-to-string message)))))
          (process-send-string channel (concat encoded-message "\n")))
      (error "Not connected to Clojure server for %s" app-name))))

(defun cloel-tcp-connection-filter (proc output app-name)
  "Handle output from the Clojure server for APP-NAME."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert output))
  (when-let ((data (condition-case err
                       (parseedn-read-str output)
                     (error (message "Error parsing output for %s: %S" app-name err) nil))))
    (if (and (hash-table-p data) (gethash :type data))
        (cl-case (gethash :type data)
          (:call-elisp-sync (cloel-handle-sync-call proc data app-name))
          (:call-elisp-async (cloel-handle-async-call data app-name))
          (:clojure-sync-return (cloel-handle-sync-return data))
          (t (message "Received unknown message type for %s: %s" app-name (gethash :type data)))))))

(defun cloel-handle-sync-call (proc data app-name)
  "Handle a call from the Clojure server for APP-NAME."
  (let* ((id (gethash :id data))
         (method (gethash :method data))
         (args (gethash :args data))
         result)
    (condition-case err
        (setq result
              (cond
               ((eq method :get-func-result)
                (if (and (stringp (car args)) (fboundp (intern (car args))))
                    (apply (intern (car args)) (car (cdr args)))
                  (error "Invalid function or arguments")))
               ((eq method :get-var-result) (symbol-value (intern (car args))))
               (t (error "Unknown method: %s" method))))
      (error (setq result (cons 'error (error-message-string err)))))
    (let ((response (make-hash-table :test 'equal)))
      (puthash :type :elisp-sync-return response)
      (puthash :id id response)
      (puthash :value (if (and (consp result) (eq (car result) 'error))
                          (format "%s" (cdr result))
                        result)
               response)
      (cloel-send-message app-name response))))

(defun cloel-handle-sync-return (data)
  "Handle synchronous call return from Clojure server."
  (let* ((call-id (gethash :id data))
         (result (gethash :result data))
         (result-promise (gethash call-id cloel-sync-call-results)))
    (when result-promise
      (puthash :result result result-promise)
      (remhash call-id cloel-sync-call-results))))

(defun cloel-handle-async-call (data app-name)
  "Handle asynchronous evaluation request from Clojure."
  (let ((func (gethash :func data))
        (args (gethash :args data)))
    (condition-case err
        (if (and (stringp func) (fboundp (intern func)))
            (apply (intern func) args)
          (error "Invalid function or arguments"))
      (error (message "Error in async eval for %s: %s" app-name (error-message-string err))))))

(defun cloel-tcp-connection-sentinel (proc event app-name)
  "Monitor the network connection for APP-NAME."
  (when (string-match "\\(closed\\|connection broken by remote peer\\)" event)
    (message "TCP connection to server for %s was closed" app-name)
    (cloel-set-app-data app-name :tcp-channel nil)))

(defun cloel-call-async (app-name func &rest args)
  "Call Clojure function FUNC with ARGS for APP-NAME."
  (cloel-send-message app-name
                      (let ((message (make-hash-table :test 'equal)))
                        (puthash :type :call-clojure-async message)
                        (puthash :func func message)
                        (puthash :args args message)
                        message)))

(defun cloel-call-sync (app-name func &rest args)
  "Synchronously call Clojure function FUNC with ARGS for APP-NAME and return the result."
  (let* ((call-id (format "%s-%s" app-name (cl-gensym)))
         (result-promise (make-hash-table :test 'equal))
         (start-time (current-time))
         result)
    (puthash call-id result-promise cloel-sync-call-results)
    (cloel-send-message app-name
                        (let ((message (make-hash-table :test 'equal)))
                          (puthash :type :call-clojure-sync message)
                          (puthash :id call-id message)
                          (puthash :func func message)
                          (puthash :args args message)
                          message))
    (cl-loop with wait-time = 0.001
             for elapsed = (float-time (time-subtract (current-time) start-time))
             do (setq result (gethash :result result-promise))
             until result
             when (> elapsed 60) do (error "Timeout waiting for sync call result")
             do (sleep-for wait-time)
             do (setq wait-time (min (* wait-time 1.5) 0.1)))
    (remhash call-id cloel-sync-call-results)
    (let ((return-value
           (if (hash-table-p result)
               (if (gethash :error result)
                   (error "Clojure error: %s" (gethash :error result))
                 (gethash :value result))
             (error "Unexpected result format: %S" result))))
      return-value)))

(provide 'cloel)
;;; cloel.el ends here
