(ns app
  (:require [cloel :as cloel]))

(defn custom-handle-clojure-call [data]
  (let [{:keys [func args]} data]
    (future
      (try
        (println "Executing Clojure function:" func "with args:" args)
        (let [result (apply (resolve (symbol func)) args)]
          (println "Clojure APP function result:" result))
        (catch Exception e
          (println "Error in Clojure call:" (.getMessage e)))))))

(alter-var-root #'cloel/handle-clojure-call (constantly custom-handle-clojure-call))

(cloel/start-server (Integer/parseInt (first *command-line-args*)))

(println "Client connected. Waiting 10 seconds before executing Elisp...")
(Thread/sleep 10000) ; Wait for 10 seconds
(println "Executing Elisp functions after 10 seconds:")
(try
  (println "Sync Result of (+ 2 3):" (cloel/elisp-eval-sync "+" 2 3))
  (println "Async Result of (+ 3 4):" @(cloel/elisp-eval-async "+" 3 4))
  (println "Value of user-full-name:" (cloel/elisp-get-var "user-full-name"))
  (catch Exception e
    (println "Error executing Elisp:" (.getMessage e))))
