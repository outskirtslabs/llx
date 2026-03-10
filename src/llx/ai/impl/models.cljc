(ns llx.ai.impl.models
  (:require
   [clojure.string :as str]
   [llx.ai.impl.models-generated :as models-generated]
   [llx.ai.impl.schema :as schema]))

(def ^:private model-registry
  models-generated/generated-models)

(defn get-model
  "Returns a model map for `provider` and `model-id`, or `nil` when not found."
  [provider model-id]
  (get-in model-registry [provider (str model-id)]))

(defn get-providers
  "Returns supported providers as a deterministic sorted vector."
  []
  (->> (keys model-registry)
       sort
       vec))

(defn get-models
  "Returns all models for `provider` as a deterministic vector sorted by `:id`."
  [provider]
  (->> (vals (get model-registry provider {}))
       (sort-by :id)
       vec))

(defn calculate-cost
  "Calculates usage cost totals for `model` rates and `usage` token counts.

  Returns a map with `:input`, `:output`, `:cache-read`, `:cache-write`, and `:total`."
  [model usage]
  (let [rates       (:cost model)
        input       (/ (* (double (or (get usage :input) 0))
                          (double (or (get rates :input) 0.0)))
                       1000000.0)
        output      (/ (* (double (or (get usage :output) 0))
                          (double (or (get rates :output) 0.0)))
                       1000000.0)
        cache-read  (/ (* (double (or (get usage :cache-read) 0))
                          (double (or (get rates :cache-read) 0.0)))
                       1000000.0)
        cache-write (/ (* (double (or (get usage :cache-write) 0))
                          (double (or (get rates :cache-write) 0.0)))
                       1000000.0)]
    {:input       input
     :output      output
     :cache-read  cache-read
     :cache-write cache-write
     :total       (+ input output cache-read cache-write)}))

(defn supports-xhigh?
  "Returns true when `model` supports xhigh reasoning effort.

  Supports explicit capability flags and known model/API compatibility rules."
  [model]
  (or (true? (get-in model [:capabilities :supports-xhigh?]))
      (str/includes? (or (:id model) "") "gpt-5.2")
      (str/includes? (or (:id model) "") "gpt-5.3")
      (str/includes? (or (:id model) "") "gpt-5.4")
      (and (= :anthropic-messages (:api model))
           (or (str/includes? (or (:id model) "") "opus-4-6")
               (str/includes? (or (:id model) "") "opus-4.6")))))

(defn models-equal?
  "Returns true when both models are non-nil and share the same `:provider` and `:id`."
  [a b]
  (and (map? a)
       (map? b)
       (= (:provider a) (:provider b))
       (= (:id a) (:id b))))

(doseq [[provider models] model-registry
        [_ model]         models]
  (when-not (schema/valid? :llx/model model)
    (throw (ex-info "Invalid built-in model" {:provider provider :model model}))))
