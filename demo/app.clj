(ns app
  (:require [cloel :as cloel]))

(defn app-start-process-confirm [data]
  ;; STEP 2: Clojure process started, call Elisp method 'cloel-demo-start-process-confirm' ASYNC.
  (cloel/elisp-eval-async "cloel-demo-start-process-confirm" (str data)))

(defn app-handle-client-connected [client-id]
  (app-start-process-confirm client-id))

(defn app-handle-client-method-call [data]
  (let [{:keys [func args]} data]
    (future
      (try
        (apply (resolve (symbol func)) args)
        (catch Exception e
          (println "Error in Clojure call:" (.getMessage e)))))))

(defn app-handle-client-message [data]
  (when (= data "Hello")
    ;; STEP 4: Handle Emacs message in Clojure sub-thread.
    (future
      ;; STEP 5: Call Elisp method result SYNC.
      (println "Sync Result of (+ 2 3):" (cloel/elisp-eval-sync "+" 2 3))
      ;; STEP 6: Get Elisp variable SYNC.
      (println "Value of 'user-full-name' is:" (cloel/elisp-get-var "user-full-name"))
      ;; STEP 7: Call Elisp method "cloel-demo-hello-confirm" ASYNC.
      (cloel/elisp-eval-async "cloel-demo-hello-confirm")
      )))

(defn app-success [message]
  ;; STEP 9: Send message to Emacs ASYNC.
  (cloel/elisp-message message))

(alter-var-root #'cloel/handle-client-method-call (constantly app-handle-client-method-call))
(alter-var-root #'cloel/handle-client-message (constantly app-handle-client-message))
(alter-var-root #'cloel/handle-client-connected (constantly app-handle-client-connected))

(cloel/start-server (Integer/parseInt (first *command-line-args*)))