+(ns cloel
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
  )
  (:import [java.io BufferedReader InputStreamReader PrintWriter OutputStreamWriter]
           [java.net ServerSocket Socket])
  (:gen-class))

(def client-connection (atom nil))
(def call-results (atom {}))
(def call-id (atom 0))

(defn send-to-client [message]
  "Send message to Emacs client with proper flushing and error handling"
  (when-let [writer (:writer @client-connection)]
    (try
      ;; pr-str: confirm compat EDN data.
      (let [line (pr-str message)]
        (when (or (str/includes? line "\n") (str/includes? line "\r"))
          (println "WARN: Message contains newline, may corrupt protocol:" line))
        (.println writer line)  ;; println 添加一个换行
        (.flush writer))
      (catch Exception e
        (println "ERROR: Failed to send to client:" (.getMessage e))
        ;; 如果发送失败，可能是连接已断开，尝试清理
        (when (.isClosed (:socket @client-connection))
          (reset! client-connection nil))))))

(defn generate-call-id []
  (swap! call-id inc))

(defn ^:export elisp-call [method & args]
  (let [id (generate-call-id)
        promise (promise)]
    (swap! call-results assoc id promise)
    (send-to-client {:type :call-elisp-sync :id id :method method :args args})
    (let [result (deref promise 60000 :timeout)]
      (swap! call-results dissoc id)
      (if (= result :timeout)
        (throw (Exception. (str "Timeout waiting for Elisp response for id: " id)))
        result))))

(defn ^:export elisp-eval-sync [func & args]
  (elisp-call :get-func-result func args))

(defn ^:export elisp-eval-async [func & args]
  (send-to-client {:type :call-elisp-async :func func :args args}))

(defn ^:export elisp-show-message [& args]
  (let [message (apply str args)]
    (elisp-eval-async "message" message)))

(defn ^:export elisp-get-var [var-name]
  (elisp-call :get-var-result var-name))

;; default data processor, covered by app
(defn ^:dynamic handle-client-async-call [data]
  (future
    (try
      (let [{:keys [func args]} data]
        (println "Default async handler:" func args)
        (if-let [f (resolve (symbol func))]
          (apply f args)
          (println "Function not found:" func)))
      (catch Exception e
        (println "ERROR in default async handler:" (.getMessage e))
        (.printStackTrace e)))))

(defn ^:dynamic handle-client-sync-call [data]
  (future
    (try
      (let [{:keys [id func args]} data
            result (try
                     {:value (apply (resolve (symbol func)) args)}
                     (catch Exception e
                       {:error (.getMessage e)}))]
        (send-to-client {:type :clojure-sync-return
                         :id id
                         :result result}))
      (catch Exception e
        (println "ERROR in sync handler:" (.getMessage e))
        (.printStackTrace e)))))

(defn ^:dynamic handle-client-message [data]
  (println "Received unhandled message type:" (pr-str data)))

(defn ^:dynamic handle-client-connected [client-id]
  (println "Client connected:" client-id))

;; match the message type: key or string
(defn- match-msg-type? [actual-type expected-type]
  (or (= actual-type expected-type)
      (= (keyword actual-type) expected-type)
      (= actual-type (name expected-type))))

;; keep-alive handle-client
(defn handle-client [^Socket client-socket]
  (let [client-id (.toString (.getRemoteSocketAddress client-socket))
        reader (BufferedReader. (InputStreamReader. (.getInputStream client-socket) "UTF-8"))
        output-stream (.getOutputStream client-socket)
        writer (PrintWriter. (OutputStreamWriter. output-stream "UTF-8") true)]
    
    (reset! client-connection {:socket client-socket :reader reader :writer writer})
    (println "[CLOEL] Client connected:" client-id)
    
    (try
      ;; mesg to app
      (handle-client-connected client-id)
      
      ;; loop to read EDN by line
      (loop []
        (when-let [line (.readLine reader)]  ;; readLine to \n 
          (try
            (let [data (edn/read-string {:readers *data-readers*} line)]
              (cond
                ;; Elisp sync
                (and (map? data) (= (:type data) :elisp-sync-return))
                (when-let [promise (get @call-results (:id data))]
                  (deliver promise (:value data)))
                
                ;; Clojure async
                (and (map? data) (= (:type data) :call-clojure-async))
                (handle-client-async-call data)
                
                ;; Clojure sync
                (and (map? data) (= (:type data) :call-clojure-sync))
                (handle-client-sync-call data)
                
                ;; other
                :else
                (handle-client-message data)))
            (catch Exception e
              (println "[CLOEL] Error processing message:" (.getMessage e))
              (println "[CLOEL] Problematic line:" line)))
          (recur)))
      
      (catch Exception e
        (println "[CLOEL] Client disconnected:" (.getMessage e)))
      
      (finally
        (println "[CLOEL] Cleaning up connection for:" client-id)
        (reset! client-connection nil)
        (try (.close client-socket) (catch Exception _))))))

(defn ^:export start-server [port]
  (let [port-num (if (string? port) (Integer/parseInt port) port)
        server-socket (ServerSocket. port-num)]
    (println "Server started on port" port-num)
    
    ;; using Thread except future to be loop long-time
    (doto (Thread. 
           (fn []
             (try
               (while true
                 (when (nil? @client-connection)
                   (let [client-socket (.accept server-socket)]
                     (println "New connection accepted")
                     ;; 为每个客户端启动独立线程
                     (doto (Thread. #(handle-client client-socket))
                       (.start)))))
               (catch Exception e
                 (println "Server accept loop error:" (.getMessage e))))))
      (.start))
    
    (println "Waiting for client connection...")))

;; keep-alive main thread, avoid CPU load
(defn ^:export keep-alive []
  (println "Main thread entering keep-alive loop...")
  (loop []
    (Thread/sleep 1000)
    ;; check the connection healthy
    (when-let [conn @client-connection]
      (when (.isClosed (:socket conn))
        (println "Detected closed connection, resetting")
        (reset! client-connection nil)))
    (recur)))