(require 'cloel)

(defvar cloel-demo-bb-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

(cloel-register-app "demo-bb" cloel-demo-bb-dir "cloel" 'bb)

(defun cloel-demo-bb-test ()
  (interactive)
  ;; STEP 1: Start Clojure process with localhost and free port.
  (cloel-demo-bb-start-process))

(defun cloel-demo-bb-start-process-confirm (client-id)
  ;; STEP 3: Send "Hello" message to Clojure process ASYNC.
  (message "Start process confirm: %s" client-id)
  (cloel-demo-bb-send-message "Hello"))

(defun cloel-demo-bb-hello-confirm ()
  ;; STEP 8: Call Clojure method "clojure.core/+" SYNC.
  (message "Got sync result of (+ 1 2 3): %s" (cloel-demo-bb-call-sync "clojure.core/+" 1 2 3))
  ;; STEP 9: Call Clojure method "app-success" ASYNC.
  (cloel-demo-bb-call-async 'app/app-success "Cloel rocks!"))
