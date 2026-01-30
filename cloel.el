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

(defvar cloel-retry-delay 2
  "Delay in seconds between connection retries.")

(defvar cloel-sync-call-results (make-hash-table :test 'equal)
  "Hash table to store sync call results.")

(defvar cloel--windows-p (eq system-type 'windows-nt)
  "Non-nil if running on Windows.")

(defvar cloel--process-kill-timeout 5
  "Seconds to wait for process termination before force killing.")

(defvar cloel--startup-delay 20
  "Seconds to wait for Clojure/JVM startup before first connect attempt.")

(defvar cloel--poll-interval 0.3
  "Seconds between port availability checks.")

(defgroup cloel nil
  "Clojure Bridge for Emacs."
  :group 'applications)

(defcustom cloel-max-retries 15
  "Maximum number of connection retries."
  :type 'integer
  :group 'cloel)

(defcustom cloel-retry-delay 3
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

(defun cloel--normalize-path (path)
  "Convert PATH to absolute form with forward slashes for JVM/Clojure."
  (let ((expanded (expand-file-name path)))
    (replace-regexp-in-string "\\\\" "/" expanded)))

(defun cloel--get-app-root (norm-path)
  "Determine the logic root directory based on input path."
  (if (file-directory-p norm-path)
      (file-name-as-directory norm-path)
    (file-name-directory norm-path)))


(defun cloel-log-error (format-string &rest args)
  "Log an error message using FORMAT-STRING and ARGS."
  (let ((message (apply #'format format-string args)))
    (message "Cloel Error: %s" message)
    message))


(defun cloel-ensure-process-killed (process)
  "Ensure PROCESS is killed safely.
On Windows, uses interrupt-process and delete-process since signals are not supported.
On Unix, uses SIGTERM then SIGKILL."
  (when (and process (process-live-p process))
    (if cloel--windows-p
        (progn
          (condition-case err
              (progn
                (interrupt-process process)
                (message "Sent interrupt signal to process on Windows")
                (sleep-for 0.5)
                (when (process-live-p process)
                  (sleep-for cloel--process-kill-timeout)
                  (when (process-live-p process)
                    (delete-process process)
                    (message "Force deleted process on Windows"))))
            (error 
             (message "Error interrupting process on Windows: %s, forcing delete" (error-message-string err))
             (delete-process process))))
      (let ((pid (process-id process)))
        (delete-process process)
        (when pid
          (ignore-errors
            (signal-process pid 'SIGTERM)
            (sleep-for 0.5)
            (when (process-running-p pid)
              (signal-process pid 'SIGKILL)))))))
  (when (processp process)
    (when (buffer-live-p (process-buffer process))
      (kill-buffer (process-buffer process)))))


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

(defun cloel-register-app (app-name app-path &optional aliases-or-bb-task clj-type)
  "Register a Cloel app. Handles directory or single .clj file."
  (let* ((norm-path (cloel--normalize-path app-path))
         (root-dir (cloel--get-app-root norm-path))
         (inferred-type (or clj-type (cloel-determine-app-type norm-path))))
    (unless inferred-type
      (error "Cloel: Cannot determine type for path %s" norm-path))
    (puthash app-name
             (list :root root-dir
                   :main-path norm-path
                   :type inferred-type
                   :aliases (or aliases-or-bb-task "cloel")
                   :port-from-file (let ((default-directory root-dir)) (cloel-get-free-port-from-port-file))
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

(defun cloel-get-free-port ()
  "Find a free port by binding to it and ensuring it remains available."
  (let (port process)
    (while (not port)
      (setq process (make-network-process :name "cloel-port-finder"
                                          :service t
                                          :host 'local
                                          :server t))
      (setq port (process-contact process :service))
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
         (setq port nil)
         (when process (delete-process process)))))
    port))

(defun cloel-determine-app-type (path)
  "Determine the type of the Clojure app based on its directory structure."
  (let ((root (cloel--get-app-root path)))
    (cond
     ((file-exists-p (expand-file-name "deps.edn" root)) 'deps)
     ((file-exists-p (expand-file-name "bb.edn" root)) 'bb)
     ((and (file-directory-p path) (directory-files path t ".*\\.clj$")) 'clojure-directory)
     ((string-suffix-p ".clj" path) 'clojure)
     (t (error "Cloel: Cannot determine type for path %s" path)))))


(defun cloel-find-main-file (app-name app-dir)
  "Find the main Clojure file for APP-NAME in APP-DIR or its src subdirectory."
  (let ((src-dir (expand-file-name "src" app-dir)))
    (cond
     ((and (file-directory-p src-dir)
           (file-exists-p (expand-file-name (format "%s.clj" app-name) src-dir)))
      (expand-file-name (format "%s.clj" app-name) src-dir))

     ((and (file-directory-p src-dir)
           (directory-files src-dir t "\\.clj$"))
      (car (directory-files src-dir t "\\.clj$")))

     ((file-exists-p (expand-file-name (format "%s.clj" app-name) app-dir))
      (expand-file-name (format "%s.clj" app-name) app-dir))

     ((file-regular-p app-dir) app-dir)

     ((directory-files app-dir t "\\.clj$")
      (car (directory-files app-dir t "\\.clj$")))

     (t (error "Cannot find .clj file for app: %s in directory: %s or src/" app-name app-dir)))))


(defun cloel--port-listening-p (host port)
  "Check if PORT is already listening on HOST.
Returns t if port is accepting connections."
  (condition-case err
      (let ((conn (open-network-stream "cloel-port-check" nil host port)))
        (when conn
          (delete-process conn)
          t))
    (error nil)))

(defun cloel--server-output-contains (app-name search-string)
  "Check if the server buffer for APP-NAME contains SEARCH-STRING."
  (let ((buf (get-buffer (format "*cloel-%s-server*" app-name))))
    (when buf
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (search-forward search-string nil t))))))

(defun cloel--wait-for-server-ready (host port timeout-seconds app-name)
  "Wait up to TIMEOUT-SECONDS for server to be ready on HOST:PORT.
Monitors both port availability and server buffer output.
Returns t if server is ready, nil otherwise."
  (let ((start-time (current-time))
        (ready nil)
        (process-died nil))
    (message "Waiting up to %d seconds for %s to be ready on port %d..." 
             timeout-seconds app-name port)
    (while (and (not ready) 
                (not process-died)
                (< (float-time (time-subtract (current-time) start-time)) timeout-seconds))
      ;; Check if process died
      (let* ((app-data (cloel-get-app-data app-name))
             (proc (plist-get app-data :server-process)))
        (unless (process-live-p proc)
          (setq process-died t)
          (message "ERROR: Server process for %s died while waiting" app-name)))
      ;; Check for successful startup message in buffer first
      (if (cloel--server-output-contains app-name "Server started on port")
          (progn
            (message "Server %s reports startup complete (message detected)" app-name)
            (setq ready t))
        ;; Fall back to port check
        (when (cloel--port-listening-p host port)
          (setq ready t)))
      (unless ready
        (sleep-for cloel--poll-interval)))
    (if ready
        (message "Server %s is ready on port %d after %.1f seconds" 
                 app-name port 
                 (float-time (time-subtract (current-time) start-time)))
      (if process-died
          (message "Server process for %s died before ready" app-name)
        (message "Timeout waiting for server %s to be ready" app-name)))
    ready))

(defun cloel-start-process (app-name)
  "Start the Clojure server using Anchor-Point and Relative Path strategy."
  (let* ((data (cloel-get-app-data app-name))
         (root-dir (plist-get data :root))
         (clj-type (plist-get data :type))
         (aliases (plist-get data :aliases))
         (port-f (plist-get data :port-from-file))
         (main-abs (cloel-find-main-file app-name (plist-get data :main-path)))
         (buf (format "*cloel-%s-server*" app-name)))

    (if port-f
        (cloel-connect-with-retry app-name "localhost" port-f)
      (let* ((port (cloel-get-free-port))
             (rel-main (file-relative-name main-abs root-dir))
             (alias-flag (cond
                          ((or (not aliases) (string-empty-p (format "%s" aliases))) "-M")
                          (t (format "-M:%s" aliases))))
             (base-cmd (pcase clj-type
                         ('bb (format "bb %s %d" aliases port))
                         (_ (format "clojure -J-Dfile.encoding=UTF-8 %s %s %d" alias-flag rel-main port))))
             ;; Simplified Windows command: avoid complex chaining that can fail silently
             (cmd (if cloel--windows-p
                      (format "cmd.exe /c chcp 65001 > nul && %s" base-cmd)
                    base-cmd)))
        
        (when (process-live-p (plist-get data :server-process))
          (cloel-stop-process app-name))

        (let ((default-directory root-dir)
              (process-connection-type nil))
          (message "Cloel: Launching [%s] with command: %s" app-name cmd)
          (let ((proc (start-process-shell-command (format "cloel-%s" app-name) buf cmd)))
            (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
            (when cloel--windows-p
              (set-process-query-on-exit-flag proc nil))
            (cloel-set-app-data app-name :server-process proc)
            (set-process-sentinel proc (lambda (p e) (cloel-server-process-sentinel p e app-name)))
            ;; Unified startup sequence for both Windows and Linux
            (run-at-time 1.0 nil #'cloel--initiate-connection-with-health-check app-name "localhost" port)))))))

(defun cloel--initiate-connection-with-health-check (app-name host port)
  "Initiate connection with health check for APP-NAME.
Waits for server to be ready, then connects with retry logic."
  (let* ((app-data (cloel-get-app-data app-name))
         (proc (plist-get app-data :server-process)))
    (if (not (process-live-p proc))
        (progn
          (message "ERROR: Clojure process for %s died prematurely, check *cloel-%s-server* buffer" app-name app-name)
          ;; Show last 20 lines of output for debugging
          (let ((buf (get-buffer (format "*cloel-%s-server*" app-name))))
            (when buf
              (with-current-buffer buf
                (message "Last output: %s" 
                         (buffer-substring (max (point-min) (- (point-max) 500)) (point-max))))))
          (cloel-set-app-data app-name :server-process nil))
      ;; Wait for server to signal ready or port to open
      (if (cloel--wait-for-server-ready host port cloel--startup-delay app-name)
          (cloel-connect-with-retry app-name host port)
        ;; Server not ready but process alive - attempt connection anyway as last resort
        (if (process-live-p proc)
            (progn
              (message "Server not detected as ready, but process is alive. Forcing connection attempt...")
              (cloel-connect-with-retry app-name host port))
          (message "ERROR: Server process died before becoming ready"))))))

(defun cloel-stop-process (app-name)
  "Stop the Clojure server process and disconnect the client for APP-NAME."
  (let* ((app-data (cloel-get-app-data app-name))
         (tcp-channel (plist-get app-data :tcp-channel))
         (server-process (plist-get app-data :server-process)))
    (when (and tcp-channel (process-live-p tcp-channel))
      (condition-case err
          (progn
            (process-send-string tcp-channel "\n")
            (sleep-for 0.1)
            (delete-process tcp-channel))
        (error (message "Error closing TCP channel: %s" (error-message-string err))))
      (cloel-set-app-data app-name :tcp-channel nil)
      (message "Disconnected Clojure TCP connection for %s" app-name))
    (when server-process
      (cloel-ensure-process-killed server-process)
      (cloel-set-app-data app-name :server-process nil)
      (message "Stopped Clojure server process for %s" app-name))))

(defun cloel-restart-process (app-name)
  "Restart the Clojure server process for APP-NAME."
  (cloel-stop-process app-name)
  (sleep-for (if cloel--windows-p 2 1))
  (cloel-start-process app-name))

(defun cloel-server-process-sentinel (process event app-name)
  "Handle Clojure process state changes for APP-NAME."
  (when (memq (process-status process) '(exit signal))
    (message "Clojure server process for %s has stopped: %s" app-name event)
    (cloel-set-app-data app-name :server-process nil)
    (let ((tcp-channel (plist-get (cloel-get-app-data app-name) :tcp-channel)))
      (when (and tcp-channel (process-live-p tcp-channel))
        (delete-process tcp-channel)
        (cloel-set-app-data app-name :tcp-channel nil)))))

(defun cloel-connect-with-retry (app-name host port)
  "Attempt to connect to the Clojure server with retries for APP-NAME."
  (let ((retries 0)
        (connected nil)
        (max-retries cloel-max-retries)
        (base-delay 0.5))
    (while (and (not connected) (< retries max-retries))
      (condition-case err
          (progn
            (cloel-connect app-name host port)
            (setq connected t)
            (message "Successfully connected to %s on attempt %d" app-name (1+ retries)))
        (error
         (setq retries (1+ retries))
         (when (< retries max-retries)
           (let ((delay (min (* base-delay (expt 1.1 retries)) 1.5)))
             (message "Connection attempt %d/%d failed for %s: %s. Retrying in %.1f seconds..."
                      retries max-retries app-name (error-message-string err) delay)
             (sleep-for delay))))))
    (unless connected
      (error "Failed to connect after %d attempts for %s" max-retries app-name))))

(defun cloel-connect (app-name host port)
  "Establish a connection to a Clojure server at HOST:PORT for APP-NAME."
  (let* ((port-num (if (stringp port) (string-to-number port) port))
         (channel (open-network-stream (format "cloel-%s-client" app-name)
                                       (format "*cloel-%s-client*" app-name)
                                       host port-num)))
    (set-process-coding-system channel 'utf-8-unix 'utf-8-unix)
    (cloel-set-app-data app-name :tcp-channel channel)

    (let ((app-data (cloel-get-app-data app-name)))
      (when (and (plist-get app-data :port-from-file)
                 (not (plist-get app-data :server-process)))
        (cloel-set-app-data app-name :server-process channel)))

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
    (if (and channel (process-live-p channel))
        (let ((encoded-message
               (condition-case err
                   (parseedn-print-str message)
                 (error
                  (message "Error encoding message: %S" err)
                  (prin1-to-string message)))))
          (let ((msg-with-newline (if (string-suffix-p "\n" encoded-message)
                                      encoded-message
                                    (concat encoded-message "\n"))))
            (process-send-string channel msg-with-newline)))
      (error "Not connected to Clojure server for %s" app-name))))

(defvar cloel--read-buffers (make-hash-table :test 'equal)
  "Hash table to store partial read buffers for each process.")

(defun cloel-tcp-connection-filter (proc output app-name)
  "Handle output from the Clojure server for APP-NAME.
Implements line-buffering to handle TCP packet fragmentation.
Handles both Unix (\\n) and Windows (\\r\\n) line endings."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert output))
  
  (let* ((proc-id (process-id proc))
         (existing-buffer (gethash proc-id cloel--read-buffers ""))
         (normalized-output (replace-regexp-in-string "\r\n" "\n" output))
         (combined (concat existing-buffer normalized-output))
         (lines nil)
         (remaining nil))
    
    (with-temp-buffer
      (insert combined)
      (goto-char (point-min))
      (while (not (eobp))
        (condition-case nil
            (progn
              (end-of-line)
              (when (looking-at "\n")
                (let ((line (buffer-substring-no-properties (line-beginning-position) (point))))
                  (push line lines))
                (forward-line)
                (setq remaining (buffer-substring-no-properties (point) (point-max)))))
          (error 
           (setq remaining (buffer-substring-no-properties (point) (point-max)))
           (goto-char (point-max))))))
    
    (puthash proc-id (or remaining "") cloel--read-buffers)
    
    (dolist (line (reverse lines))
      (when (and line (not (string-blank-p line)))
        (condition-case err
            (when-let ((data (parseedn-read-str line)))
              (if (and (hash-table-p data) (gethash :type data))
                  (cl-case (gethash :type data)
                    (:call-elisp-sync (cloel-handle-sync-call proc data app-name))
                    (:call-elisp-async (cloel-handle-async-call data app-name))
                    (:clojure-sync-return (cloel-handle-sync-return data))
                    (t (message "Received unknown message type for %s: %s" 
                               app-name (gethash :type data))))
                (message "Received message without type for %s: %S" app-name data)))
          (error 
           (message "Cloel: Error parsing EDN line for %s: %S | Line: %s" 
                   app-name err line)))))))

(defun cloel-clear-read-buffer (proc)
  "Clear the read buffer for a process."
  (remhash (process-id proc) cloel--read-buffers))

(defun cloel-tcp-connection-sentinel (proc event app-name)
  "Monitor the network connection for APP-NAME."
  (when (string-match "\\(closed\\|connection broken by remote peer\\|failed\\)" event)
    (message "TCP connection to server for %s was closed or failed: %s" app-name event)
    (cloel-clear-read-buffer proc)
    (cloel-set-app-data app-name :tcp-channel nil)
    (let ((app-data (cloel-get-app-data app-name)))
      (when (eq (plist-get app-data :server-process) proc)
        (cloel-set-app-data app-name :server-process nil)))))


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



(defun cloel-call-async (app-name func &rest args)
  "Call Clojure function FUNC with ARGS for APP-NAME.
FUNC should be a string naming the function in Clojure namespace."
  (let ((message (make-hash-table :test 'equal)))
    (puthash :type :call-clojure-async message)
    (puthash :func (if (symbolp func) (symbol-name func) (format "%s" func)) message)
    (puthash :args (apply #'vector args) message)
    (cloel-send-message app-name message)))


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
             when (> elapsed cloel-sync-timeout) do (error "Timeout waiting for sync call result")
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
