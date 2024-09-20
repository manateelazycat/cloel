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

(defn -main [& args]
  (let [port (Integer/parseInt (first args))]
    (println "Starting app server on port" port)
    (cloel/start-server port)))

(apply -main *command-line-args*)
