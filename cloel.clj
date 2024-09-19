(ns tcp-server
  (:import [java.net ServerSocket Socket]
           [java.io BufferedReader InputStreamReader PrintWriter]))

(def client-connection (atom nil))

(defn send-to-client [message]
  (when-let [writer (:writer @client-connection)]
    (.println writer message)
    (.flush writer)))

(defn handle-client [^Socket client-socket]
  (let [client-id (.toString (.getRemoteSocketAddress client-socket))
        reader (BufferedReader. (InputStreamReader. (.getInputStream client-socket)))
        writer (PrintWriter. (.getOutputStream client-socket))]
    (reset! client-connection {:socket client-socket :reader reader :writer writer})
    (println "Client connected:" client-id)
    (try
      (while true
        (let [input (.readLine reader)]
          (if input
            (do
              (println "Received from client:" input)
              (send-to-client (str "Server echoes: " input)))
            (throw (Exception. "Client disconnected")))))
      (catch Exception e
        (println "Client disconnected:" (.getMessage e))
        (reset! client-connection nil)
        (.close client-socket)))))

(defn start-server [port]
  (let [server-socket (ServerSocket. port)]
    (println "Server started on port" port)
    (future
      (while true
        (when (nil? @client-connection)
          (let [client-socket (.accept server-socket)]
            (handle-client client-socket)))))
    (loop []
      (let [input (read-line)]
        (when input
          (if @client-connection
            (do
              (println "Server sending:" input)
              (send-to-client (str "Server says: " input)))
            (println "No client connected. Message not sent."))
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
