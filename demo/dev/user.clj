(ns user
  (:require cloel.repl))

(defn start []
  (cloel.repl/start-server))

(defn stop []
  (cloel.repl/stop-server))

(start)

(comment
  (start)
  (stop))
