(ns ol.llx.ai.impl.registry
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [ol.llx.ai.impl.schema :as schema]))

(def adapters-key :ol.llx.registry/adapters)
(def tools-key :ol.llx.registry/tools)

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

(>defn resolve-registry
       ([]
        [=> :ol.llx/registry-map]
        (resolve-registry (default-registry)))
       ([registry]
        [any? => :ol.llx/registry-map]
        (cond
          (nil? registry) (resolve-registry (default-registry))
          (instance? MutableRegistry registry) (resolve-registry @(:registry* registry))
          (instance? DynamicRegistry registry) (if (some? *registry*)
                                                 (resolve-registry *registry*)
                                                 (resolve-registry (default-registry)))
          (map? registry) (immutable-registry registry)
          :else (throw (ex-info "Unsupported registry implementation" {:registry registry})))))

(>defn register-adapter
       ([registry adapter]
        [any? :ol.llx/adapter => :ol.llx/registry-map]
        (register-adapter registry adapter nil))
       ([registry adapter source-id]
        [any? :ol.llx/adapter any? => :ol.llx/registry-map]
        (let [registry (resolve-registry registry)
              adapter  (schema/assert-valid! :ol.llx/adapter adapter)]
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

(>defn get-adapter
       ([api]
        [:ol.llx/api => [:maybe :ol.llx/adapter]]
        (get-adapter (default-registry) api))
       ([registry api]
        [any? :ol.llx/api => [:maybe :ol.llx/adapter]]
        (schema/assert-valid! :ol.llx/api api)
        (get-in (resolve-registry registry) [adapters-key api :adapter])))

(defn get-adapters
  ([]
   (get-adapters (default-registry)))
  ([registry]
   (mapv :adapter (vals (get (resolve-registry registry) adapters-key {})))))
