(require 'ert)

(ert-deftest cloel-find-main-file-test ()
  "Test finding main files in different scenarios."
  (let ((temp-dir (make-temp-file "cloel-test-" t)))
    (unwind-protect
        (progn
          ;; Test with file in src/
          (make-directory (expand-file-name "src" temp-dir))
          (with-temp-file (expand-file-name "src/test-app.clj" temp-dir)
            (insert "(ns test-app)\n(defn -main [& args])"))
          (should (string= (file-name-nondirectory (cloel-find-main-file "test-app" temp-dir))
                           "test-app.clj"))

          ;; Test with file in app-dir
          (delete-directory (expand-file-name "src" temp-dir) t)
          (with-temp-file (expand-file-name "test-app.clj" temp-dir)
            (insert "(ns test-app)\n(defn -main [& args])"))
          (should (string= (file-name-nondirectory (cloel-find-main-file "test-app" temp-dir))
                           "test-app.clj")))
      (delete-directory temp-dir t))))

(ert-deftest cloel-determine-app-type-test ()
  "Test determination of app types."
  (let ((temp-dir (make-temp-file "cloel-test-" t)))
    (unwind-protect
        (progn
          ;; Test deps.edn app
          (with-temp-file (expand-file-name "deps.edn" temp-dir)
            (insert "{:deps {}}"))
          (should (eq (cloel-determine-app-type temp-dir) 'deps))

          ;; Test bb.edn app
          (delete-file (expand-file-name "deps.edn" temp-dir))
          (with-temp-file (expand-file-name "bb.edn" temp-dir)
            (insert "{:tasks {}}"))
          (should (eq (cloel-determine-app-type temp-dir) 'bb))

          ;; Test clojure-directory app
          (delete-file (expand-file-name "bb.edn" temp-dir))
          (with-temp-file (expand-file-name "sample.clj" temp-dir)
            (insert "(ns sample)"))
          (should (eq (cloel-determine-app-type temp-dir) 'clojure-directory)))
      (delete-directory temp-dir t))))

(ert-deftest cloel-register-app-test ()
  "Test registration of apps."
  (let ((cloel-apps (make-hash-table :test 'equal))
        (temp-dir (make-temp-file "cloel-test-" t)))
    (unwind-protect
        (progn
          ;; Create a deps.edn file
          (with-temp-file (expand-file-name "deps.edn" temp-dir)
            (insert "{:deps {}}"))

          ;; Register the app
          (cloel-register-app "test-app" temp-dir)

          ;; Check it was registered properly
          (let ((app-data (cloel-get-app-data "test-app")))
            (should app-data)
            (should (eq (plist-get app-data :type) 'deps))
            (should (string= (plist-get app-data :dir) temp-dir))))
      (delete-directory temp-dir t))))

(ert-deftest cloel-set-app-data-test ()
  "Test setting app data."
  (let ((cloel-apps (make-hash-table :test 'equal)))
    ;; Register a dummy app
    (puthash "test-app" '(:dir "/tmp" :type deps) cloel-apps)

    ;; Set some data
    (cloel-set-app-data "test-app" :port 8080)

    ;; Check it was set correctly
    (should (= (plist-get (cloel-get-app-data "test-app") :port) 8080))))

(ert-deftest cloel-get-free-port-test ()
  "Test getting a free port."
  (let ((port (cloel-get-free-port)))
    (should (numberp port))
    (should (> port 1024))))
