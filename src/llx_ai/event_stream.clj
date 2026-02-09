(ns llx-ai.event-stream
  (:import
   [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(set! *warn-on-reflection* true)

(def ^:private end-marker ::end-of-stream)

(defn create
  [is-complete? extract-result]
  {:queue          (LinkedBlockingQueue.)
   :closed?        (atom false)
   :is-complete?   is-complete?
   :extract-result extract-result
   :result*        (promise)})

(defn assistant-message-stream
  []
  (create
   (fn [event]
     (contains? #{:done :error} (:type event)))
   (fn [event]
     (:assistant-message event))))

(defn push!
  [stream event]
  (let [{:keys [queue closed? is-complete? extract-result result*]} stream
        terminal?                                                   (is-complete? event)]
    (when-not @closed?
      (if terminal?
        (when (compare-and-set! closed? false true)
          (deliver result* (extract-result event))
          (.put ^LinkedBlockingQueue queue event)
          (.put ^LinkedBlockingQueue queue end-marker))
        (.put ^LinkedBlockingQueue queue event)))
    stream))

(defn end!
  [stream]
  (let [{:keys [queue closed?]} stream]
    (when (compare-and-set! closed? false true)
      (.put ^LinkedBlockingQueue queue end-marker))
    stream))

(defn take!
  ([stream]
   (take! stream nil))
  ([stream timeout-ms]
   (let [{:keys [queue]} stream
         item            (if (number? timeout-ms)
                           (.poll ^LinkedBlockingQueue queue (long timeout-ms) TimeUnit/MILLISECONDS)
                           (.take ^LinkedBlockingQueue queue))]
     (when (and item (not= item end-marker))
       item))))

(defn drain!
  [stream]
  (loop [events []]
    (if-let [event (take! stream)]
      (recur (conj events event))
      events)))

(defn result
  [stream]
  @(get stream :result*))
