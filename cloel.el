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

(defun cloel-generate-app-functions (app-name)
  "Generate app-specific functions for APP-NAME."
  (let ((start-func-name (intern (format "cloel-%s-start-process" app-name)))
        (stop-func-name (intern (format "cloel-%s-stop-process" app-name)))
        (restart-func-name (intern (format "cloel-%s-restart-process" app-name)))
        (call-async-func-name (intern (format "cloel-%s-call-async" app-name)))
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

    (fset send-message-func-name
          `(lambda (message)
             (cloel-send-message ',app-name message)))

    (list start-func-name stop-func-name restart-func-name call-async-func-name send-message-func-name)))

(defun cloel-register-app (app-name app-file)
  "Register an app with APP-NAME and APP-FILE."
  (puthash app-name
           (list :file app-file
                 :clojure-process nil
                 :server-process nil)
           cloel-apps)
  (cloel-generate-app-functions app-name))

(defun cloel-get-app-data (app-name)
  "Get the data for APP-NAME."
  (gethash app-name cloel-apps))

(defun cloel-set-app-data (app-name key value)
  "Set KEY to VALUE for APP-NAME."
  (when-let ((app-data (cloel-get-app-data app-name)))
    (puthash app-name (plist-put app-data key value) cloel-apps)))

(defun cloel-get-free-port ()
  "Find a free port."
  (let ((process (make-network-process :name "cloel-port-finder"
                                       :service t
                                       :host 'local
                                       :server t)))
    (prog1 (process-contact process :service)
      (delete-process process))))

(defun cloel-start-process (app-name)
  "Start the Clojure server process for APP-NAME."
  (let* ((app-data (cloel-get-app-data app-name))
         (app-file (plist-get app-data :file))
         (port (cloel-get-free-port)))
    (unless (file-exists-p app-file)
      (error "Cannot find app file at %s" app-file))
    (let ((process (start-process (format "cloel-%s-clojure-server" app-name)
                                  (format "*cloel-%s-clojure-server*" app-name)
                                  "clojure" "-M" app-file (number-to-string port))))
      (cloel-set-app-data app-name :clojure-process process)
      (message "Starting Clojure server for %s on port %d" app-name port)
      (set-process-sentinel process
                            (lambda (proc event)
                              (cloel-clojure-process-sentinel proc event app-name)))
      (cloel-connect-with-retry app-name "localhost" port))))

(defun cloel-stop-process (app-name)
  "Stop the Clojure server process and disconnect the client for APP-NAME."
  (let* ((app-data (cloel-get-app-data app-name))
         (server-process (plist-get app-data :server-process))
         (clojure-process (plist-get app-data :clojure-process)))
    (when (process-live-p server-process)
      (delete-process server-process)
      (cloel-set-app-data app-name :server-process nil)
      (message "Disconnected from Clojure server for %s" app-name))
    (when (process-live-p clojure-process)
      (delete-process clojure-process)
      (cloel-set-app-data app-name :clojure-process nil)
      (message "Stopped Clojure server process for %s" app-name))))

(defun cloel-restart-process (app-name)
  "Restart the Clojure server process for APP-NAME."
  (cloel-stop-process app-name)
  (sleep-for 1)
  (cloel-start-process app-name))

(defun cloel-clojure-process-sentinel (process event app-name)
  "Handle Clojure process state changes for APP-NAME."
  (when (memq (process-status process) '(exit signal))
    (message "Clojure process for %s has stopped: %s" app-name event)
    (cloel-set-app-data app-name :clojure-process nil)))

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
         (process (open-network-stream (format "cloel-%s-client" app-name)
                                       (format "*cloel-%s-client*" app-name)
                                       host port-num)))
    (cloel-set-app-data app-name :server-process process)
    (set-process-coding-system process 'utf-8 'utf-8)
    (if (process-live-p process)
        (progn
          (set-process-filter process
                              (lambda (proc output)
                                (cloel-process-filter proc output app-name)))
          (set-process-sentinel process
                                (lambda (proc event)
                                  (cloel-process-sentinel proc event app-name)))
          (message "Connected to Clojure server for %s at %s:%s" app-name host port-num))
      (error "Failed to connect to Clojure server for %s at %s:%s" app-name host port-num))))

(defun cloel-send-message (app-name message)
  "Send MESSAGE to the connected Clojure server for APP-NAME."
  (let ((process (plist-get (cloel-get-app-data app-name) :server-process)))
    (if (process-live-p process)
        (let ((encoded-message
               (condition-case err
                   (parseedn-print-str message)
                 (error
                  (message "Error encoding message: %S" err)
                  (prin1-to-string message)))))
          (process-send-string process (concat encoded-message "\n")))
      (error "Not connected to Clojure server for %s" app-name))))

(defun cloel-process-filter (proc output app-name)
  "Handle output from the Clojure server for APP-NAME."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert output))
  (when-let ((data (condition-case err
                       (parseedn-read-str output)
                     (error (message "Error parsing output for %s: %S" app-name err) nil))))
    (if (and (hash-table-p data) (gethash :type data))
        (cl-case (gethash :type data)
          (:call (cloel-handle-call proc data app-name))
          (:message (message "Server for %s says: %s" app-name (gethash :content data)))
          (t (message "Received unknown message type for %s: %s" app-name (gethash :type data)))))))

(defun cloel-handle-call (proc data app-name)
  "Handle a call from the Clojure server for APP-NAME."
  (let* ((id (gethash :id data))
         (method (gethash :method data))
         (args (gethash :args data))
         result)
    (condition-case err
        (setq result
              (cond
               ((eq method :eval)
                (if (and (stringp (car args)) (fboundp (intern (car args))))
                    (apply (intern (car args)) (car (cdr args)))
                  (error "Invalid function or arguments")))
               ((eq method :get-var) (symbol-value (intern (car args))))
               (t (error "Unknown method: %s" method))))
      (error (setq result (cons 'error (error-message-string err)))))
    (let ((response (make-hash-table :test 'equal)))
      (puthash :type :sync-return response)
      (puthash :id id response)
      (puthash :value (if (and (consp result) (eq (car result) 'error))
                          (format "%s" (cdr result))
                        result)
               response)
      (cloel-send-message app-name response))))

(defun cloel-process-sentinel (proc event app-name)
  "Monitor the network connection for APP-NAME."
  (when (string-match "\\(closed\\|connection broken by remote peer\\)" event)
    (message "Connection to server for %s was closed" app-name)
    (cloel-set-app-data app-name :server-process nil)))

(defun cloel-call-async (app-name func &rest args)
  "Call Clojure function FUNC with ARGS for APP-NAME."
  (cloel-send-message app-name
                      (let ((message (make-hash-table :test 'equal)))
                        (puthash :type :async-call message)
                        (puthash :func func message)
                        (puthash :args args message)
                        message)))

(provide 'cloel)
;;; cloel.el ends here
