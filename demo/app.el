(require 'cloel)

(defvar cloel-demo-clj-file (expand-file-name "app.clj" (file-name-directory load-file-name)))

(cloel-register-app "demo" cloel-demo-clj-file)

;; (cloel-demo-start-process)
;; (cloel-demo-stop-process)
;; (cloel-demo-restart-process)
;; (cloel-demo-call-clojure "clojure.core/+" 1 2 3)
;; (cloel-demo-send-message "hello")
