(ns cloel.repl
  (:require
   cloel
   [clojure.java.io :as io])
  (:import [java.net ServerSocket]))

(def port-file-name ".cloel-port")

(defn- random-available-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(def server (atom nil))

(defn- delete-port-file []
  (let [file (io/file port-file-name)]
    (when (.exists file)
      (io/delete-file file))))

(defn stop-server []
  (when-let [s @server]
    (.close s)
    (reset! server nil))
  (delete-port-file))

(defn start-server []
  (stop-server)
  (let [port (random-available-port)]
    (spit port-file-name port)
    (reset! server (cloel/start-server port))))

(comment
  (start-server)
  (stop-server))
