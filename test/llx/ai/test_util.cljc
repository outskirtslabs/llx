(ns llx.ai.test-util
  (:require
   #?@(:clj [[babashka.json :as json]
             [clojure.edn :as edn]
             [llx.ai.impl.client.stream :as stream]]
       :cljs [[cljs.reader :as reader]
              [promesa.core :as p]
              ["node:fs" :as fs]])
   [promesa.exec.csp :as sp]
   [taoensso.trove :as trove]))

(defn read-text-file
  [path]
  #?(:clj (slurp path)
     :cljs (.readFileSync fs path "utf8")))

(defn read-edn-file
  [path]
  #?(:clj (edn/read-string (read-text-file path))
     :cljs (reader/read-string (read-text-file path))))

(defn json-write
  [x]
  #?(:clj (json/write-str x)
     :cljs (.stringify js/JSON (clj->js x))))

(defn json-read
  ([s]
   (json-read s nil))
  ([s _opts]
   #?(:clj (json/read-str s {:key-fn keyword})
      :cljs (js->clj (.parse js/JSON s) :keywordize-keys true))))

(defn json-read-safe
  ([s]
   (json-read-safe s nil))
  ([s opts]
   #?(:clj (try
             (json-read s opts)
             (catch Exception _
               nil))
      :cljs (try
              (json-read s opts)
              (catch :default _
                nil)))))

(defn now-ms
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

#?(:clj
   (defn await!
     ([x]
      (stream/await! x))
     ([x duration default-on-timeout]
      (stream/await! x duration default-on-timeout))))

#?(:cljs
   (defn await!
     ([x]
      (await! x nil nil))
     ([_x _duration-ms _default-on-timeout]
      (throw (ex-info "util/await! is JVM-only; use cljs.test/async with promesa in CLJS tests." {})))))

(defn submap?
  "Is m1 a subset of m2?"
  [m1 m2]
  (if (and (map? m1) (map? m2))
    (every? (fn [[k v]]
              (and (contains? m2 k)
                   (submap? v (get m2 k))))
            m1)
    (= m1 m2)))

(defn submap-debug?
  "Is m1 a subset of m2?
   Print missing keys or mismatched values."
  [m1 m2]
  (if (and (map? m1) (map? m2))
    (every? (fn [[k v]]
              (when-not (contains? m2 k)
                (println "m1 has key, m2 does not:" k))
              (and (contains? m2 k)
                   (submap-debug? v (get m2 k))))
            m1)
    (if (= m1 m2)
      true
      (do
        (println "Nested values don't match, m1 val=" m1 "m2 val=" m2)
        false))))

(defn- force-value
  [x]
  #?(:clj (if (instance? clojure.lang.IDeref x) @x x)
     :cljs (if (satisfies? IDeref x) @x x)))

(defn- capture-log-event!
  [logs* args]
  (let [[ns loc level id opts] args
        event (cond
                (and (= 1 (count args))
                     (map? (force-value (first args))))
                (force-value (first args))

                :else
                (merge {:ns    (force-value ns)
                        :loc   (force-value loc)
                        :level (force-value level)
                        :id    (force-value id)}
                       (or (force-value opts) {})))]
    (swap! logs* conj event)
    nil))

#?(:cljs
   (defn- thenable?
     [x]
     (and (some? x) (fn? (aget x "then")))))

(defn with-captured-logs!
  [f]
  #?(:clj
     (let [logs* (atom [])]
       (binding [trove/*log-fn* (fn [& args]
                                  (capture-log-event! logs* args))]
         (f logs*)))
     :cljs
     (let [logs*     (atom [])
           previous* trove/*log-fn*
           restore!  (fn [] (set! trove/*log-fn* previous*))]
       (set! trove/*log-fn* (fn [& args]
                              (capture-log-event! logs* args)))
       (let [result (f logs*)]
         (if (thenable? result)
           (-> result
               (p/finally restore!))
           (do
             (restore!)
             result))))))

(defn first-event
  [logs* event-id]
  (first (filter #(= event-id (:id %)) @logs*)))

(defn strip-generated
  ([event]
   (strip-generated event []))
  ([event data-keys]
   (cond-> (dissoc event
                   :ns
                   :host
                   :file
                   :line
                   :column
                   :loc
                   :msg
                   :error)
     (seq data-keys) (update :data #(apply dissoc % data-keys)))))

(defn collect-channel-events!
  [ch timeout-ms]
  #?(:clj
     (deref
      (future
        (loop [events []]
          (if-let [event (sp/take! ch)]
            (recur (conj events event))
            events)))
      timeout-ms
      ::timeout)
     :cljs
     (let [deadline-ms (+ (now-ms) timeout-ms)
           timeout-token (js-obj)]
       (loop [events []]
         (let [remaining-ms (- deadline-ms (now-ms))]
           (if (<= remaining-ms 0)
             ::timeout
             (let [event (await! (p/race [(sp/take ch)
                                          (p/delay remaining-ms timeout-token)]))]
               (cond
                 (identical? event timeout-token) ::timeout
                 (nil? event) events
                 :else (recur (conj events event))))))))))
