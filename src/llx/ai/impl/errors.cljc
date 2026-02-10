(ns llx.ai.impl.errors
  (:require
   [clojure.string :as str]
   [taoensso.trove :as trove]))

(def client-errors
  #{:llx/authentication-error
    :llx/authorization-error
    :llx/invalid-request
    :llx/model-not-found
    :llx/quota-exceeded
    :llx/content-filter
    :llx/unsupported-reasoning-level
    :llx/tool-not-found
    :llx/validation-error})

(def transient-errors
  #{:llx/rate-limit
    :llx/timeout
    :llx/connection-error
    :llx/server-error})

(def response-errors
  #{:llx/invalid-response
    :llx/streaming-error})

(def all-error-types
  (into #{} (concat client-errors transient-errors response-errors #{:llx/provider-error})))

(defn- build-error-data
  [error-type message & {:keys [provider http-status provider-code retry-after
                                recoverable? request-id context]
                         :or   {recoverable? false}}]
  (cond-> {:type         error-type
           :message      message
           :recoverable? recoverable?}
    provider      (assoc :provider provider)
    http-status   (assoc :http-status http-status)
    provider-code (assoc :provider-code provider-code)
    retry-after   (assoc :retry-after retry-after)
    request-id    (assoc :request-id request-id)
    context       (assoc :context context)))

(defn rate-limit
  [provider message & {:keys [http-status retry-after request-id provider-code]}]
  (ex-info message
           (build-error-data :llx/rate-limit message
                             :provider provider
                             :http-status (or http-status 429)
                             :provider-code provider-code
                             :retry-after retry-after
                             :recoverable? true
                             :request-id request-id)))

(defn server-error
  [provider message & {:keys [http-status provider-code request-id]}]
  (ex-info message
           (build-error-data :llx/server-error message
                             :provider provider
                             :http-status http-status
                             :provider-code provider-code
                             :recoverable? true
                             :request-id request-id)))

(defn timeout-error
  [provider message & {:keys [timeout-ms request-id]}]
  (ex-info message
           (build-error-data :llx/timeout message
                             :provider provider
                             :recoverable? true
                             :request-id request-id
                             :context (when timeout-ms {:timeout-ms timeout-ms}))))

(defn connection-error
  [provider message & {:keys [request-id]}]
  (ex-info message
           (build-error-data :llx/connection-error message
                             :provider provider
                             :recoverable? true
                             :request-id request-id)))

(defn authentication-error
  [provider message & {:keys [http-status]}]
  (ex-info message
           (build-error-data :llx/authentication-error message
                             :provider provider
                             :http-status (or http-status 401)
                             :recoverable? false)))

(defn authorization-error
  [provider message & {:keys [http-status]}]
  (ex-info message
           (build-error-data :llx/authorization-error message
                             :provider provider
                             :http-status (or http-status 403)
                             :recoverable? false)))

(defn invalid-request
  [message & {:keys [provider http-status context]}]
  (ex-info message
           (build-error-data :llx/invalid-request message
                             :provider provider
                             :http-status http-status
                             :recoverable? false
                             :context context)))

(defn model-not-found
  [provider message & {:keys [http-status]}]
  (ex-info message
           (build-error-data :llx/model-not-found message
                             :provider provider
                             :http-status (or http-status 404)
                             :recoverable? false)))

(defn quota-exceeded
  [provider message & {:keys [http-status]}]
  (ex-info message
           (build-error-data :llx/quota-exceeded message
                             :provider provider
                             :http-status (or http-status 429)
                             :recoverable? false)))

(defn content-filter
  [provider message & {:keys [provider-code]}]
  (ex-info message
           (build-error-data :llx/content-filter message
                             :provider provider
                             :provider-code provider-code
                             :recoverable? false)))

(defn invalid-response
  [provider message & {:keys [context]}]
  (ex-info message
           (build-error-data :llx/invalid-response message
                             :provider provider
                             :recoverable? false
                             :context context)))

(defn streaming-error
  [provider message & {:keys [recoverable?]}]
  (ex-info message
           (build-error-data :llx/streaming-error message
                             :provider provider
                             :recoverable? (boolean recoverable?))))

(defn provider-error
  [provider message & {:keys [http-status provider-code request-id recoverable?]}]
  (ex-info message
           (build-error-data :llx/provider-error message
                             :provider provider
                             :http-status http-status
                             :provider-code provider-code
                             :recoverable? (boolean recoverable?)
                             :request-id request-id)))

(defn unsupported-reasoning-level
  [model-id requested-level]
  (let [msg (str "Model " model-id " does not support reasoning level " (name requested-level))]
    (ex-info msg
             {:type            :llx/unsupported-reasoning-level
              :message         msg
              :model-id        model-id
              :requested-level requested-level
              :recoverable?    false})))

(defn tool-not-found
  [tool-name available-tools]
  (let [sorted (sort available-tools)
        msg    (str "Tool not found: " tool-name ". Available tools: " (pr-str sorted))]
    (ex-info msg
             {:type            :llx/tool-not-found
              :message         msg
              :tool-name       tool-name
              :available-tools sorted
              :recoverable?    false})))

(defn validation-error
  [tool-name args errors]
  (let [msg (str "Validation failed for tool " tool-name ": "
                 (pr-str errors) ". "
                 "Received arguments: " (pr-str args))]
    (ex-info msg
             {:type         :llx/validation-error
              :message      msg
              :tool-name    tool-name
              :arguments    args
              :errors       errors
              :recoverable? false})))

(defn http-status->error
  [status provider message & {:keys [provider-code retry-after request-id body]}]
  (let [status (long status)
        quota? (or (str/includes? (str/lower-case (str body)) "quota")
                   (str/includes? (str/lower-case (str message)) "quota"))]
    (case status
      400 (invalid-request message :provider provider :http-status status :context {:body body})
      401 (authentication-error provider message :http-status status)
      403 (authorization-error provider message :http-status status)
      404 (model-not-found provider message :http-status status)
      408 (timeout-error provider message :request-id request-id)
      429 (if (and quota? (nil? retry-after))
            (quota-exceeded provider message :http-status status)
            (rate-limit provider message :http-status status
                        :retry-after retry-after :request-id request-id
                        :provider-code provider-code))
      (500 502 503 504) (server-error provider message :http-status status
                                      :provider-code provider-code :request-id request-id)
      (provider-error provider message :http-status status
                      :provider-code provider-code :request-id request-id
                      :recoverable? (>= status 500)))))

(defn- parse-retry-after-value
  [raw header-name]
  (when (and raw (string? raw) (seq raw))
    (try
      (let [v (#?(:clj Double/parseDouble :cljs js/parseFloat) raw)]
        (if (= header-name "retry-after-ms")
          (/ v 1000.0)
          v))
      (catch #?(:clj Exception :cljs :default) _
        nil))))

(defn extract-retry-after
  [headers & {:keys [max-seconds] :or {max-seconds 60}}]
  (let [[header-name raw] (or (when-let [v (get headers "retry-after-ms")]
                                ["retry-after-ms" v])
                              (when-let [v (get headers "retry-after")]
                                ["retry-after" v])
                              (when-let [v (get headers "x-ratelimit-reset-after")]
                                ["x-ratelimit-reset-after" v]))]
    (when-let [seconds (parse-retry-after-value raw header-name)]
      (when (pos? seconds)
        (min seconds (double max-seconds))))))

(defn extract-retry-after-from-message
  [message & {:keys [max-seconds] :or {max-seconds 60}}]
  (let [text (some-> message str)]
    (when-let [[_ raw] (and (seq (or text ""))
                            (re-find #"(?i)please\s+retry\s+in\s+([0-9]+(?:\.[0-9]+)?)s" text))]
      (when-let [seconds (parse-retry-after-value raw "retry-after")]
        (when (pos? seconds)
          (min seconds (double max-seconds)))))))

(defn extract-retry-after-hint
  [headers message & {:keys [max-seconds] :or {max-seconds 60}}]
  (or (extract-retry-after headers :max-seconds max-seconds)
      (extract-retry-after-from-message message :max-seconds max-seconds)))

(defn llx-error?
  [ex]
  (and (instance? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex)
       (contains? all-error-types (:type (ex-data ex)))))

(defn recoverable?
  [ex]
  (and (llx-error? ex)
       (get (ex-data ex) :recoverable? false)))

(defn rate-limit-error?
  [ex]
  (and (llx-error? ex)
       (= :llx/rate-limit (:type (ex-data ex)))))

(defn quota-exceeded-error?
  [ex]
  (and (llx-error? ex)
       (= :llx/quota-exceeded (:type (ex-data ex)))))

(defn rate-limited-error?
  [ex]
  (or (rate-limit-error? ex)
      (quota-exceeded-error? ex)))

(defn timeout-error?
  [ex]
  (and (llx-error? ex)
       (= :llx/timeout (:type (ex-data ex)))))

(defn client-error?
  [ex]
  (and (llx-error? ex)
       (contains? client-errors (:type (ex-data ex)))))

(defn transient-error?
  [ex]
  (and (llx-error? ex)
       (contains? transient-errors (:type (ex-data ex)))))

(defn should-retry?
  [ex & {:keys [max-retries current-retry] :or {max-retries 2 current-retry 0}}]
  (and (< current-retry max-retries)
       (recoverable? ex)
       (contains? transient-errors (:type (ex-data ex)))))

(defn retry-delay-ms
  [ex retry-count]
  (let [data (ex-data ex)]
    (cond
      (and (rate-limit-error? ex) (:retry-after data))
      (long (* 1000 (:retry-after data)))

      (rate-limit-error? ex)
      (long (* 1000 (Math/pow 2 retry-count)))

      (= :llx/server-error (:type data))
      (+ (long (* 1000 (Math/pow 2 retry-count)))
         (rand-int 1000))

      :else
      (long (* 1000 (inc retry-count))))))

(defn retry-loop
  ([f max-retries sleep-fn]
   (retry-loop f max-retries sleep-fn {}))
  ([f max-retries sleep-fn {:keys [call-id provider]}]
   (loop [attempt 0]
     (let [result (try
                    {:ok (f)}
                    (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
                      (if (should-retry? e :max-retries max-retries
                                         :current-retry attempt)
                        {:retry e}
                        (throw e))))]
       (if-let [ex (:retry result)]
         (let [delay-ms (retry-delay-ms ex attempt)
               exd      (ex-data ex)]
           (trove/log! {:level :info
                        :id    :llx.obs/retry-scheduled
                        :data  {:call-id      call-id
                                :attempt      attempt
                                :next-attempt (inc attempt)
                                :max-retries  max-retries
                                :delay-ms     delay-ms
                                :error-type   (:type exd)
                                :provider     (or provider (:provider exd))
                                :request-id   (:request-id exd)
                                :retry-after  (:retry-after exd)}})
           (sleep-fn delay-ms)
           (recur (inc attempt)))
         (:ok result))))))
