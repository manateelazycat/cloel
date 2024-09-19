(ns tcp-server
  (:import [java.net ServerSocket Socket]
           [java.io BufferedReader InputStreamReader PrintWriter]))

(defn handle-client [^Socket client-socket]
  (with-open [reader (BufferedReader. (InputStreamReader. (.getInputStream client-socket)))
              writer (PrintWriter. (.getOutputStream client-socket))]
    (loop []
      (when-let [input (.readLine reader)]
        (println "Received:" input)
        (.println writer (str "Echo: " input))
        (.flush writer)
        (recur)))))

(defn start-server [port]
  (let [server-socket (ServerSocket. port)]
    (println "Server started on port" port)
    (while true
      (let [client-socket (.accept server-socket)]
        (future (handle-client client-socket))))))

(defn parse-port [args]
  (if (seq args)
    (try
      (Integer/parseInt (first args))
      (catch NumberFormatException _
        (println "Invalid port number. Using default port 8080.")
        8080))
    8080))

(defn -main [& args]
  (let [port (parse-port args)]
    (println "Starting TCP server on port" port)
    (start-server port)))

(apply -main *command-line-args*)
