(require 'cloel)

(defvar cloel-demo-dir (file-name-directory load-file-name))

(cloel-register-app "demo" cloel-demo-dir "cloel" 'clojure)

(defun cloel-demo-test ()
  (interactive)
  ;; STEP 1: Start Clojure process with localhost and free port.
  (cloel-demo-start-process))

(defun cloel-demo-start-process-confirm (client-id)
  ;; STEP 3: Send "Hello" message to Clojure process ASYNC.
  (message "Start process confirm: %s" client-id)
  (cloel-demo-send-message "Hello"))

(defun cloel-demo-hello-confirm ()
  ;; STEP 8: Call Clojure method "clojure.core/+" SYNC.
  (message "Got sync result of (+ 1 2 3): %s" (cloel-demo-call-sync "clojure.core/+" 1 2 3))
  ;; STEP 9: Call Clojure method "app-success" ASYNC.
  (cloel-demo-call-async 'app/app-success "Cloel rocks!"))
