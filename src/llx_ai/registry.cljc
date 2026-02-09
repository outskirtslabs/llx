(ns llx-ai.registry
  (:require
   [llx-ai.schema :as schema]))

(defonce ^:private adapter-registry* (atom {}))

(defn register!
  ([adapter]
   (register! adapter nil))
  ([adapter source-id]
   (let [adapter (schema/assert-valid! :llx/adapter adapter)]
     (swap! adapter-registry* assoc (:api adapter) {:adapter adapter :source-id source-id})
     adapter)))

(defn get-adapter
  [api]
  (get-in @adapter-registry* [api :adapter]))

(defn get-adapters
  []
  (mapv :adapter (vals @adapter-registry*)))

(defn unregister-source!
  [source-id]
  (swap! adapter-registry*
         (fn [entries]
           (reduce-kv
            (fn [acc api entry]
              (if (= source-id (:source-id entry))
                acc
                (assoc acc api entry)))
            {}
            entries))))

(defn clear!
  []
  (reset! adapter-registry* {}))
