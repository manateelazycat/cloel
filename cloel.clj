(ns tcp-server
  (:import [java.net ServerSocket Socket]
           [java.io BufferedReader InputStreamReader PrintWriter]))

(def clients (atom {}))

(defn send-to-all-clients [message]
  (doseq [[socket writer] @clients]
    (.println writer message)
    (.flush writer)))

(defn handle-client [^Socket client-socket]
  (let [client-id (.toString (.getRemoteSocketAddress client-socket))
        reader (BufferedReader. (InputStreamReader. (.getInputStream client-socket)))
        writer (PrintWriter. (.getOutputStream client-socket))]
    (swap! clients assoc client-socket writer)
    (try
      (while true
        (let [input (.readLine reader)]
          (if input
            (do
              (println "Received from" client-id ":" input)
              (send-to-all-clients (str "Client " client-id " says: " input)))
            (throw (Exception. "Client disconnected")))))
      (catch Exception e
        (println "Client" client-id "disconnected:" (.getMessage e))
        (swap! clients dissoc client-socket)
        (.close client-socket)))))

(defn start-server [port]
  (let [server-socket (ServerSocket. port)]
    (println "Server started on port" port)
    (future
      (while true
        (let [client-socket (.accept server-socket)]
          (future (handle-client client-socket)))))
    (loop []
      (let [input (read-line)]
        (when input
          (println "Server broadcasting:" input)
          (send-to-all-clients (str "Server says: " input))
          (recur))))))

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
