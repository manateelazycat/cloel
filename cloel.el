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
(defun cloel-get-free-port ()
  (save-excursion
    (let* ((process-buffer " *cloel-temp*")
           (process (make-network-process
                     :name process-buffer
                     :buffer process-buffer
                     :family 'ipv4
                     :server t
                     :host "127.0.0.1"
                     :service t))
           (port (process-contact process :service)))
      (delete-process process)
      (kill-buffer process-buffer)
      (format "%s" port))))

(defvar cloel-server-process nil
  "The network process connected to the Clojure server.")

(defvar cloel-receive-hook nil
  "Hook run when a message is received from the server.")

(defun cloel-connect (host port)
  "Establish a connection to a Clojure server at HOST:PORT."
  (let ((port-num (if (stringp port) (string-to-number port) port)))
    (setq cloel-server-process
          (open-network-stream "cloel-client" "*cloel-client*" host port-num))
    (if (process-live-p cloel-server-process)
        (progn
          (set-process-filter cloel-server-process 'cloel-process-filter)
          (set-process-sentinel cloel-server-process 'cloel-process-sentinel)
          (message "Connected to Clojure server at %s:%s" host port-num))
      (error "Failed to connect to Clojure server at %s:%s" host port-num))))

(defun cloel-disconnect ()
  "Disconnect from the Clojure server."
  (when (process-live-p cloel-server-process)
    (delete-process cloel-server-process)
    (setq cloel-server-process nil)
    (message "Disconnected from Clojure server")))

(defun cloel-send-message (message)
  "Send MESSAGE to the connected Clojure server."
  (if (process-live-p cloel-server-process)
      (process-send-string cloel-server-process (concat message "\n"))
    (error "Not connected to Clojure server")))

(defun cloel-process-filter (proc output)
  "Handle output from the Clojure server."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert output))
  (message "Received from server: %s" (string-trim output))
  (run-hook-with-args 'cloel-receive-hook output))

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

(defun my-message-handler (message)
  (message "Custom handler received: %s" message))
(cloel-add-receive-hook 'my-message-handler)

(provide 'cloel)

;;; cloel.el ends here
