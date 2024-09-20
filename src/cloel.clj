(ns cloel
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

(defn elisp-eval-sync [func & args]
  (elisp-call :eval func args))

(defn elisp-eval-async [func & args]
  (let [id (generate-call-id)]
    (println "Async calling Elisp function:" func "with args:" args "and id:" id)  ; Debug print
    (send-to-client {:type :call :id id :method :eval-async :func func :args args})
    (future
      (let [result (apply elisp-call :eval-async func args)]
        result))))

(defn elisp-message [& args]
  (let [message (apply str args)]
    (elisp-eval-async "message" message)))

(defn elisp-get-var [var-name]
  (elisp-call :get-var var-name))

(defn ^:dynamic handle-clojure-call [data]
  (let [{:keys [func args]} data]
    (println "Executing Clojure function:" func "with args:" args)))

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
              (cond
                (and (map? data) (= (:type data) :return))
                (when-let [promise (.get call-results (:id data))]
                  (deliver promise (:value data)))

                (and (map? data) (= (:type data) :clojure-call))
                (handle-clojure-call data)

                :else
                (println "Received message:" data)))
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
    (loop []
      (let [input (read-line)]
        (when input
          (recur))))))
