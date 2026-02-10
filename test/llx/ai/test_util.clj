(ns llx.ai.test-util
  (:require
   [taoensso.trove :as trove]))

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

(defmacro with-captured-logs
  [[logs*] & body]
  `(let [~logs* (atom [])]
     (binding [trove/*log-fn* (fn [& args#]
                                (let [force#
                                      (fn [x#]
                                        (if (instance? clojure.lang.IDeref x#) @x# x#))
                                      [ns# loc# level# id# opts#]
                                      args#

                                      event#
                                      (cond
                                        (and (= 1 (count args#))
                                             (map? (force# (first args#))))
                                        (force# (first args#))

                                        :else
                                        (merge {:ns    (force# ns#)
                                                :loc   (force# loc#)
                                                :level (force# level#)
                                                :id    (force# id#)}
                                               (or (force# opts#) {})))]
                                  (swap! ~logs* conj event#))
                                nil)]
       ~@body)))

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
