(ns hooks.llx-ai.test-util
  (:require
   [clj-kondo.hooks-api :as api]))

(defn with-captured-logs
  [{:keys [node]}]
  (let [[_ binding-vec & body] (:children node)
        binding-form           (first (:children binding-vec))]
    {:node (api/list-node
            (list* (api/token-node 'let)
                   (api/vector-node [binding-form (api/token-node 'nil)])
                   body))}))
