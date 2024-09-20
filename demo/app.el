(require 'cloel)

(defvar cloel-demo-clj-file (expand-file-name "app.clj" (file-name-directory load-file-name)))

(cloel-register-app "demo" cloel-demo-clj-file)

(defun cloel-demo-test ()
  (interactive)
  ;; STEP 1: Start Clojure process with localhost and free port.
  (cloel-demo-start-process))

(defun cloel-demo-start-process-confirm (client-id)
  ;; STEP 3: Send "Hello" message to Clojure process ASYNC.
  (message "Start process confirm: %s" client-id)
  (cloel-demo-send-message "Hello"))

(defun cloel-demo-hello-confirm ()
  ;; STEP 8: Call Clojure method "app-success" ASYNC.
  (cloel-demo-call-clojure "app-success" "Cloel rocks!"))
