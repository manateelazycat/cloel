(ns tcp-server
  (:require [clojure.edn :as edn])
  (:import [java.net ServerSocket Socket]
           [java.io BufferedReader InputStreamReader PrintWriter]
           [java.util.concurrent ConcurrentHashMap]))

(def client-connection (atom nil))
(def call-results (ConcurrentHashMap.))
(def call-id (atom 0))

(defn send-to-client [message]
  (when-let [writer (:writer @client-connection)]
    (println "Sending to client:" message)  ; Debug print
    (.println writer (pr-str message))
    (.flush writer)))

(defn generate-call-id []
  (swap! call-id inc))

(defn elisp-call [method & args]
  (let [id (generate-call-id)
        promise (promise)]
    (.put call-results id promise)
    (println "Calling Elisp method:" method "with args:" args "and id:" id)  ; Debug print
    (send-to-client {:type :call :id id :method method :args args})
    (let [result (deref promise 60000 :timeout)]  ; 60 second timeout
      (.remove call-results id)
      (if (= result :timeout)
        (do
          (println "Timeout waiting for Elisp response for id:" id)  ; Debug print
          (throw (Exception. (str "Timeout waiting for Elisp response for id: " id))))
        (do
          (println "Received result for id:" id ":" result)  ; Debug print
          result)))))

(defn elisp-eval-sync [form]
  (elisp-call :eval form))

(defn elisp-eval-async [func-name & args]
  (let [id (generate-call-id)]
    (println "Async calling Elisp function:" func-name "with args:" args "and id:" id)  ; Debug print
    (send-to-client {:type :call :id id :method :eval-async :func func-name :args args})
    (future
      (let [result (apply elisp-call :eval-async func-name args)]
        result))))

(defn elisp-get-var [var-name]
  (elisp-call :get-var var-name))

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
            (let [data (edn/read-string input)]
              (println "Received from client:" data)
              (when (and (map? data) (= (:type data) :return))
                (when-let [promise (.get call-results (:id data))]
                  (deliver promise (:value data)))))
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
            (future (handle-client client-socket))))))
    (println "Waiting for client connection...")
    (while (nil? @client-connection)
      (Thread/sleep 100))
    (println "Client connected. Waiting 10 seconds before executing Elisp...")
    (Thread/sleep 10000) ; Wait for 10 seconds
    (println "Executing Elisp functions after 10 seconds:")
    (try
      (println "Sync Result of (+ 2 3):" (elisp-eval-sync '(+ 2 3)))
      (println "Async Result of (+ 3 4):" @(elisp-eval-async "+" 3 4))
      (println "Value of user-full-name:" (elisp-get-var "user-full-name"))
      (catch Exception e
        (println "Error executing Elisp:" (.getMessage e))))
    (loop []
      (let [input (read-line)]
        (when input
          (if @client-connection
            (do
              (println "Server sending:" input)
              (send-to-client {:type :message :content input}))
            (println "No client connected. Message not sent."))
          (recur))))))

(defn -main [& args]
  (let [port (if (seq args)
               (Integer/parseInt (first args))
               8080)]
    (println "Starting TCP server on port" port)
    (start-server port)))

(apply -main *command-line-args*)
