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


;;; Code:
(require 'cl-lib)
(require 'parseedn)

(defvar cloel-server-process nil
  "The network process connected to the Clojure server.")

(defvar cloel-clojure-process nil
  "The Clojure server process.")

(defvar cloel-receive-hook nil
  "Hook run when a message is received from the server.")

(defvar cloel-max-retries 5
  "Maximum number of connection retries.")

(defvar cloel-retry-delay 1
  "Delay in seconds between connection retries.")

(defvar cloel-clojure-file
  (expand-file-name "cloel.clj" (file-name-directory load-file-name))
  "Path to the cloel.clj file.")

(defun cloel-get-free-port ()
  "Find a free port."
  (let ((process (make-network-process :name "cloel-port-finder"
                                       :service t
                                       :host 'local
                                       :server t)))
    (prog1 (process-contact process :service)
      (delete-process process))))

(defun cloel-start-process ()
  "Start the Clojure server process."
  (let ((port (cloel-get-free-port)))
    (unless (file-exists-p cloel-clojure-file)
      (error "Cannot find cloel.clj at %s" cloel-clojure-file))
    (setq cloel-clojure-process
          (start-process "cloel-clojure-server" "*cloel-clojure-server*"
                         "clojure" "-M" cloel-clojure-file (number-to-string port)))
    (message "Starting Clojure server on port %d" port)
    (set-process-sentinel cloel-clojure-process 'cloel-clojure-process-sentinel)
    (cloel-connect-with-retry "localhost" port)))

(defun cloel-stop-process ()
  "Stop the Clojure server process and disconnect the client."
  (when (process-live-p cloel-server-process)
    (delete-process cloel-server-process)
    (setq cloel-server-process nil)
    (message "Disconnected from Clojure server"))
  (when (process-live-p cloel-clojure-process)
    (delete-process cloel-clojure-process)
    (setq cloel-clojure-process nil)
    (message "Stopped Clojure server process")))

(defun cloel-restart-process ()
  "Restart the Clojure server process."
  (cloel-stop-process)
  (sleep-for 1)        ; Give the system a moment to free up resources
  (cloel-start-process))

(defun cloel-clojure-process-sentinel (process event)
  "Handle Clojure process state changes."
  (when (memq (process-status process) '(exit signal))
    (message "Clojure process has stopped: %s" event)
    (setq cloel-clojure-process nil)))

(defun cloel-connect-with-retry (host port)
  "Attempt to connect to the Clojure server with retries."
  (let ((retries 0)
        (connected nil))
    (while (and (not connected) (< retries cloel-max-retries))
      (condition-case err
          (progn
            (cloel-connect host port)
            (setq connected t))
        (error
         (setq retries (1+ retries))
         (message "Connection attempt %d failed: %s" retries (error-message-string err))
         (when (< retries cloel-max-retries)
           (message "Retrying in %d seconds..." cloel-retry-delay)
           (sleep-for cloel-retry-delay)))))
    (unless connected
      (error "Failed to connect after %d attempts" cloel-max-retries))))

(defun cloel-connect (host port)
  "Establish a connection to a Clojure server at HOST:PORT."
  (let ((port-num (if (stringp port) (string-to-number port) port)))
    (setq cloel-server-process
          (open-network-stream "cloel-client" "*cloel-client*" host port-num))
    (set-process-coding-system cloel-server-process 'utf-8 'utf-8)
    (if (process-live-p cloel-server-process)
        (progn
          (set-process-filter cloel-server-process 'cloel-process-filter)
          (set-process-sentinel cloel-server-process 'cloel-process-sentinel)
          (message "Connected to Clojure server at %s:%s" host port-num))
      (error "Failed to connect to Clojure server at %s:%s" host port-num))))

(defun cloel-disconnect ()
  "Disconnect from the Clojure server and stop the Clojure process."
  (when (process-live-p cloel-server-process)
    (delete-process cloel-server-process)
    (setq cloel-server-process nil)
    (message "Disconnected from Clojure server"))
  (when (process-live-p cloel-clojure-process)
    (delete-process cloel-clojure-process)
    (setq cloel-clojure-process nil)
    (message "Stopped Clojure server process")))

(defun cloel-send-message (message)
  "Send MESSAGE to the connected Clojure server."
  (if (process-live-p cloel-server-process)
      (progn
        (message "Sending to server: %S" message) ; Debug print
        (let ((encoded-message
               (condition-case err
                   (parseedn-print-str message)
                 (error
                  (message "Error encoding message: %S" err)
                  (prin1-to-string message)))))
          (process-send-string cloel-server-process (concat encoded-message "\n"))))
    (error "Not connected to Clojure server")))

(defun cloel-process-filter (proc output)
  "Handle output from the Clojure server."
  (message "Raw output received: %S" output) ; Debug print
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert output))
  (let ((data (condition-case err
                  (parseedn-read-str output)
                (error (message "Error parsing output: %S" err) nil))))
    (message "Parsed data: %S" data)    ; Debug print
    (when data
      (if (and (hash-table-p data) (gethash :type data))
          (cl-case (gethash :type data)
            (:call (cloel-handle-call proc data))
            (:message (message "Server says: %s" (gethash :content data)))
            (t (message "Received unknown message type: %s" (gethash :type data))))
        (progn
          (message "Received from server: %s" (if (stringp data) (string-trim data) data))
          (run-hook-with-args 'cloel-receive-hook output))))))

(defun cloel-handle-call (proc data)
  (let* ((id (gethash :id data))
         (method (gethash :method data))
         (args (gethash :args data))
         result)
    (message "Handling call: %S" data)  ; Debug print
    (condition-case err
        (setq result
              (cond
               ((eq method :eval)
                (if (and (stringp (car args)) (fboundp (intern (car args))))
                    (apply (intern (car args)) (car (cdr args)))
                  (error "Invalid function or arguments")))
               ((eq method :eval-async)
                (if (and (stringp (car args)) (fboundp (intern (car args))))
                    (apply (intern (car args)) (cdr args))
                  (error "Invalid function or arguments")))
               ((eq method :get-var) (symbol-value (intern (car args))))
               (t (error "Unknown method: %s" method))))
      (error (setq result (cons 'error (error-message-string err)))))
    (message "Call result: %S" result)  ; Debug print
    (let ((response (make-hash-table :test 'equal)))
      (puthash :type :return response)
      (puthash :id id response)
      (puthash :value (if (and (consp result) (eq (car result) 'error))
                          (format "%s" (cdr result))
                        result)
               response)
      (message "Sending response: %S" response) ; Debug print
      (cloel-send-message response))))

(defun cloel-process-sentinel (proc event)
  "Monitor the network connection."
  (when (string-match "\\(closed\\|connection broken by remote peer\\)" event)
    (message "Connection to server was closed")
    (setq cloel-server-process nil)))

(defun cloel-add-receive-hook (func)
  "Add a function to be called when a message is received."
  (add-hook 'cloel-receive-hook func))

(defun cloel-remove-receive-hook (func)
  "Remove a function from the receive hook."
  (remove-hook 'cloel-receive-hook func))

(defun cloel-call-clojure (func &rest args)
  "Call Clojure function FUNC with ARGS."
  (cloel-send-message
   (let ((message (make-hash-table :test 'equal)))
     (puthash :type :clojure-call message)
     (puthash :func func message)
     (puthash :args args message)
     message)))

(defun my-message-handler (message)
  (message "Custom handler received: %s" message))
(cloel-add-receive-hook 'my-message-handler)

(provide 'cloel)
;;; cloel.el ends here
