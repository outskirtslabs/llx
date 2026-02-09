(ns llx-ai.registry
  (:require
   [llx-ai.schema :as schema]))

(def adapters-key :llx.registry/adapters)
(def tools-key :llx.registry/tools)

(defrecord MutableRegistry [registry*])
(defrecord DynamicRegistry [])

(def ^:dynamic *registry* nil)

(defn immutable-registry
  ([] (immutable-registry {}))
  ([registry]
   (-> (or registry {})
       (update adapters-key #(or % {}))
       (update tools-key #(or % {})))))

(defn mutable-registry
  [registry*]
  (->MutableRegistry registry*))

(defn dynamic-registry
  []
  (->DynamicRegistry))

(defonce ^:private default-registry* (atom (immutable-registry)))

(declare resolve-registry)

(defn default-registry
  []
  @default-registry*)

(defn set-default-registry!
  [registry]
  (reset! default-registry* (resolve-registry registry)))

(defn resolve-registry
  ([]
   (resolve-registry (default-registry)))
  ([registry]
   (cond
     (nil? registry) (resolve-registry (default-registry))
     (instance? MutableRegistry registry) (resolve-registry @(:registry* registry))
     (instance? DynamicRegistry registry) (if (some? *registry*)
                                            (resolve-registry *registry*)
                                            (resolve-registry (default-registry)))
     (map? registry) (immutable-registry registry)
     :else (throw (ex-info "Unsupported registry implementation" {:registry registry})))))

(defn register-adapter
  ([registry adapter]
   (register-adapter registry adapter nil))
  ([registry adapter source-id]
   (let [registry (resolve-registry registry)
         adapter  (schema/assert-valid! :llx/adapter adapter)]
     (assoc-in registry [adapters-key (:api adapter)] {:adapter adapter :source-id source-id}))))

(defn unregister-adapters-by-source
  [registry source-id]
  (let [entries (get (resolve-registry registry) adapters-key {})]
    (assoc (resolve-registry registry)
           adapters-key
           (reduce-kv
            (fn [acc api entry]
              (if (= source-id (:source-id entry))
                acc
                (assoc acc api entry)))
            {}
            entries))))

(defn clear-adapters
  [registry]
  (assoc (resolve-registry registry) adapters-key {}))

(defn get-adapter
  ([api]
   (get-adapter (default-registry) api))
  ([registry api]
   (get-in (resolve-registry registry) [adapters-key api :adapter])))

(defn get-adapters
  ([]
   (get-adapters (default-registry)))
  ([registry]
   (mapv :adapter (vals (get (resolve-registry registry) adapters-key {})))))
