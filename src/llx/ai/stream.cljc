(ns llx.ai.stream
  (:require
   [taoensso.trove :as trove]))

(def ^:private stream-type ::stream)
(def ^:private pending-result ::pending-result)

(defn- safe-callback!
  [handler arg]
  (try
    (handler arg)
    (catch #?(:clj Exception :cljs :default) ex
      (trove/log! {:level :warn
                   :id    :llx.obs/stream-callback-error
                   :data  {:error-message (ex-message ex)}}))))

(defn close!
  "Internal producer API: closes the stream once and notifies consumer."
  [stream close-meta]
  (let [{:keys [state* lock]} stream
        consumer*             (atom nil)]
    (locking lock
      (when-not (:close-meta @state*)
        (reset! consumer* (:consumer @state*))
        (swap! state* (fn [state]
                        (-> state
                            (assoc :close-meta close-meta)
                            (assoc :consumer nil))))))
    (when-let [f (get @consumer* :on-close)]
      (safe-callback! f close-meta))
    stream))

(defn stream?
  "Returns true when `x` is an LLX stream handle."
  [x]
  (= stream-type (:llx.ai.stream/type x)))

(defn create
  "Creates a stream handle.

  Internal constructor used by runtime wiring."
  ([]
   (create {}))
  ([{:keys [cancel-fn start-fn] :as opts}]
   (let [now-ms (or (:clock/now-ms opts) (constantly 0))]
     (cond-> {:llx.ai.stream/type stream-type
              :state*             (atom {:consumer        nil
                                         :consumed?       false
                                         :started?        false
                                         :result          pending-result
                                         :close-meta      nil
                                         :cancel-invoked? false})
              :clock/now-ms       now-ms
              :lock               (atom nil)}
       cancel-fn (assoc :cancel-fn cancel-fn)
       start-fn (assoc :start-fn start-fn)))))

(defn consume!
  "Attaches a single consumer to `stream` and starts production.

  May be called exactly once per stream handle; subsequent calls throw
  `ex-info` with `:type :llx/stream-already-consumed`."
  [stream handlers]
  (let [{:keys [state* lock start-fn]} stream
        should-start?                  (atom false)
        terminal*                      (atom nil)]
    (locking lock
      (let [{:keys [consumed? started? result close-meta]} @state*]
        (when consumed?
          (throw (ex-info "Stream already consumed"
                          {:type :llx/stream-already-consumed})))
        (swap! state* assoc :consumer (or handlers {}) :consumed? true)
        (when (and start-fn (not started?) (nil? close-meta))
          (swap! state* assoc :started? true)
          (reset! should-start? true))
        (when (or (not= pending-result result) close-meta)
          (reset! terminal* {:result result :close-meta close-meta}))))
    (when @should-start?
      (try
        (start-fn)
        (catch #?(:clj Exception :cljs :default) ex
          ;; Producer start failure is normalized to a close signal.
          (close! stream {:reason       :error
                          :error        ex
                          :timestamp-ms ((:clock/now-ms stream))}))))
    (when-let [{:keys [result close-meta]} @terminal*]
      (when (and (not= pending-result result)
                 (:on-result handlers))
        (safe-callback! (:on-result handlers) result))
      (when (and close-meta (:on-close handlers))
        (safe-callback! (:on-close handlers) close-meta)))
    stream))

(defn consumed?
  "Returns true when a consumer has already been attached."
  [stream]
  (boolean (:consumed? @(:state* stream))))

(defn closed?
  "Returns true when the stream has been closed."
  [stream]
  (boolean (:close-meta @(:state* stream))))

(defn cancel!
  "Cancels a stream.

  Idempotent; invokes runtime cancel hook at most once and closes the stream
  with reason `:cancelled`."
  [stream]
  (let [{:keys [state* cancel-fn lock]} stream
        now-ms                          (or (:clock/now-ms stream) (constantly 0))
        run-cancel?                     (atom false)
        consumer*                       (atom nil)
        close-meta*                     (atom nil)]
    (locking lock
      (swap! state*
             (fn [state]
               (if (:cancel-invoked? state)
                 state
                 (do
                   (reset! run-cancel? true)
                   (assoc state :cancel-invoked? true)))))
      (when @run-cancel?
        (when-not (:close-meta @state*)
          (let [close-meta {:reason       :cancelled
                            :error        nil
                            :timestamp-ms (now-ms)}]
            (reset! consumer* (:consumer @state*))
            (reset! close-meta* close-meta)
            (swap! state* (fn [state]
                            (-> state
                                (assoc :close-meta close-meta)
                                (assoc :consumer nil))))))))
    (when @run-cancel?
      (when cancel-fn
        (try
          (cancel-fn)
          (catch #?(:clj Exception :cljs :default) ex
            (trove/log! {:level :warn
                         :id    :llx.obs/stream-cancel-error
                         :data  {:error-message (ex-message ex)}}))))
      (when (and @close-meta* (get @consumer* :on-close))
        (safe-callback! (:on-close @consumer*) @close-meta*)))
    stream))

(defn emit-event!
  "Internal producer API: emits one event and notifies active consumer."
  [stream event]
  (let [{:keys [state* lock]} stream
        consumer*             (atom nil)]
    (locking lock
      (when-not (:close-meta @state*)
        (reset! consumer* (:consumer @state*))))
    (if-let [f (get @consumer* :on-event)]
      (safe-callback! f event)
      (trove/log! {:level :warn
                   :id    :llx.obs/stream-event-without-consumer
                   :data  {:event-type (:type event)}}))
    stream))

(defn emit-result!
  "Internal producer API: emits terminal assistant message once."
  [stream assistant-message]
  (let [{:keys [state* lock]} stream
        consumer*             (atom nil)]
    (locking lock
      (when (= pending-result (:result @state*))
        (swap! state* assoc :result assistant-message)
        (reset! consumer* (:consumer @state*))))
    (when-let [f (get @consumer* :on-result)]
      (safe-callback! f assistant-message))
    stream))

