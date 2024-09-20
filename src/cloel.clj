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
    (.println writer (pr-str message))
    (.flush writer)))

(defn generate-call-id []
  (swap! call-id inc))

(defn ^:export elisp-call [method & args]
  (let [id (generate-call-id)
        promise (promise)]
    (.put call-results id promise)
    (send-to-client {:type :call :id id :method method :args args})
    (let [result (deref promise 60000 :timeout)]  ; 60 second timeout
      (.remove call-results id)
      (if (= result :timeout)
        (throw (Exception. (str "Timeout waiting for Elisp response for id: " id)))
        result))))

(defn ^:export elisp-eval-sync [func & args]
  (elisp-call :eval func args))

(defn ^:export elisp-eval-async [func & args]
  (future
    (elisp-call :eval func args)
    ))

(defn ^:export elisp-show-message [& args]
  (let [message (apply str args)]
    (elisp-eval-async "message" message)))

(defn ^:export elisp-get-var [var-name]
  (elisp-call :get-var var-name))

(defn ^:dynamic handle-client-method-call [data]
  (let [{:keys [func args]} data]
    (println "Executing Clojure function:" func "with args:" args)))

(defn ^:dynamic handle-client-message [data]
  (println "Received message:" data))

(defn ^:dynamic handle-client-connected [client-id]
  (println "Client connected:" client-id))

(defn handle-client [^Socket client-socket]
  (let [client-id (.toString (.getRemoteSocketAddress client-socket))
        reader (BufferedReader. (InputStreamReader. (.getInputStream client-socket)))
        writer (PrintWriter. (.getOutputStream client-socket))]
    (reset! client-connection {:socket client-socket :reader reader :writer writer})
    (handle-client-connected client-id)
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

                (and (map? data) (= (:type data) :call-async))
                (handle-client-method-call data)

                :else
                (handle-client-message data)
                ))
            (throw (Exception. "Client disconnected")))))
      (catch Exception e
        (println "Client disconnected:" (.getMessage e))
        (reset! client-connection nil)
        (.close client-socket)))))

(defn ^:export start-server [port]
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
