(ns cloel
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
  "Send message to Emacs client with null-check and error recovery"
  (let [conn @client-connection]
    (if (and conn (:writer conn))
      (try
        (let [line (pr-str message)
              writer (:writer conn)]
          ;; 防御性检查换行符
          (when (or (str/includes? line "\n") (str/includes? line "\r"))
            (println "[SEND-WARN] Message contains newline:" (subs line 0 (min 50 (count line)))))
          (.println writer line)
          (.flush writer)
          true)  ;; 返回成功
        (catch Exception e
          (println "[SEND-ERROR] Failed to send:" (.getMessage e))
          ;; 仅在实际异常时重置连接，而非 preemptively
          (when (instance? java.net.SocketException e)
            (println "[SEND] Resetting connection due to socket error")
            (reset! client-connection nil))
          false))
      (do
        (println "[SEND-ERROR] No active connection or writer is nil")
        false))))
	
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
  (try
    (send-to-client {:type :call-elisp-async :func func :args args})
    (catch Exception e
      (println "[ASYNC-ERROR] elisp-eval-async failed:" (.getMessage e)))))

(defn ^:export elisp-show-message [& args]
  (let [message (apply str args)]
    (elisp-eval-async "message" message)))

(defn ^:export elisp-get-var [var-name]
  (elisp-call :get-var-result var-name))

;; 默认处理器（将被应用程序覆盖）
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

;; 关键修复：健壮的类型匹配（同时接受关键字和字符串）
(defn- match-msg-type? [actual-type expected-type]
  (or (= actual-type expected-type)
      (= (keyword actual-type) expected-type)
      (= actual-type (name expected-type))))

;; 在 handle-client 中确保异常不会杀死连接处理循环
(defn handle-client [^Socket client-socket]
  (let [client-id (.toString (.getRemoteSocketAddress client-socket))
        reader (BufferedReader. (InputStreamReader. (.getInputStream client-socket) "UTF-8"))
        writer (PrintWriter. (OutputStreamWriter. (.getOutputStream client-socket) "UTF-8") true)]
    
    ;; 原子操作设置连接
    (reset! client-connection {:socket client-socket :reader reader :writer writer})
    (println "[CONN] Handler started for:" client-id)
    
    (try
      ;; 通知连接建立
      (handle-client-connected client-id)
      
      ;; 读取循环：确保即使单条消息处理失败也不中断连接
      (loop []
        (when-let [line (try (.readLine reader) 
                             (catch Exception e 
                               (println "[READ-ERROR]" (.getMessage e))
                               nil))]
          (if (str/blank? line)
            (recur)  ;; 跳过空行
            (do
              (try
                (let [data (edn/read-string {:readers *data-readers*} line)]
                  (cond
                    ;; 处理同步返回（来自 Emacs 的响应）
                    (and (map? data) (= (:type data) :elisp-sync-return))
                    (when-let [promise (get @call-results (:id data))]
                      (deliver promise (:value data)))
                    
                    ;; 处理异步调用
                    (and (map? data) (= (:type data) :call-clojure-async))
                    (do (println "[RECV] Async call:" (:func data))
                        (handle-client-async-call data))
                    
                    ;; 处理同步调用
                    (and (map? data) (= (:type data) :call-clojure-sync))
                    (do (println "[RECV] Sync call:" (:func data))
                        (handle-client-sync-call data))
                    
                    ;; 其他类型
                    :else
                    (handle-client-message data)))
                (catch Exception e
                  (println "[PARSE-ERROR] Failed to parse line:" (.getMessage e))
                  (println "[PARSE-ERROR] Line content:" (subs line 0 (min 100 (count line))))))
              (recur)))))
      
      (catch Exception e
        (println "[CONN] Client disconnected or error:" (.getMessage e)))
      
      (finally
        (println "[CONN] Cleaning up connection for:" client-id)
        ;; 关键：仅清理当前 socket 的连接
        (when (= (:socket @client-connection) client-socket)
          (reset! client-connection nil))
        (try (.close client-socket) (catch Exception _))))))

(defn ^:export start-server [port]
  (let [port-num (if (string? port) (Integer/parseInt port) port)
        server-socket (ServerSocket. port-num)]
    (println "Server started on port" port-num)
    
    ;; 使用线程而非 future，确保长期运行
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

;; 保持主线程存活（修正：避免占用 CPU）
(defn ^:export keep-alive []
  (println "Main thread entering keep-alive loop...")
  (loop []
    (Thread/sleep 1000)
    ;; 检查连接健康状态（可选）
    (when-let [conn @client-connection]
      (when (.isClosed (:socket conn))
        (println "Detected closed connection, resetting")
        (reset! client-connection nil)))
    (recur)))