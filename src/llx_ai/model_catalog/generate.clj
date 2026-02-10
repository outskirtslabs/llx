(ns llx-ai.model-catalog.generate
  (:require
   [babashka.http-client :as http]
   [babashka.json :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [llx-ai.schema :as schema]))

(def supported-provider-rules
  {"openai"    {:provider :openai :api :openai-responses :base-url "https://api.openai.com/v1"}
   "anthropic" {:provider :anthropic :api :anthropic-messages :base-url "https://api.anthropic.com"}
   "google"    {:provider :google :api :google-generative-ai :base-url "https://generativelanguage.googleapis.com/v1beta"}
   "mistral"   {:provider :mistral :api :openai-completions :base-url "https://api.mistral.ai/v1"}})

(def generated-artifact-path "src/llx_ai/models_generated.cljc")
(def overrides-path "dev/llx_ai/model_catalog/overrides.edn")
(def models-dev-url "https://models.dev/api.json")

(defn fetch-models-dev-data
  "Fetches and decodes the models.dev catalog JSON as a Clojure map."
  []
  (let [response (http/get models-dev-url {:throw false})
        status   (:status response)]
    (when-not (= 200 status)
      (throw (ex-info "Failed to fetch models.dev catalog"
                      {:status status :url models-dev-url :body (:body response)})))
    (json/read-str (:body response))))

(defn- key->string
  [k]
  (cond
    (string? k) k
    (keyword? k) (if-let [ns (namespace k)]
                   (str ns "/" (name k))
                   (name k))
    :else (str k)))

(defn- finite-number
  [n]
  (let [v (double (or n 0.0))]
    (if (Double/isFinite v) v 0.0)))

(defn- non-negative-int
  [n default-v]
  (let [v (long (or n default-v))]
    (if (neg? v) default-v v)))

(defn- supported-input
  [model]
  (let [raw-input (or (get-in model ["modalities" "input"])
                      (get-in model [:modalities :input])
                      [])
        image?    (contains? (set raw-input) "image")]
    (if image?
      (sorted-set :text :image)
      (sorted-set :text))))

(defn- normalize-cost
  [model]
  {:input       (finite-number (or (get-in model ["cost" "input"])
                                   (get-in model [:cost :input])))
   :output      (finite-number (or (get-in model ["cost" "output"])
                                   (get-in model [:cost :output])))
   :cache-read  (finite-number (or (get-in model ["cost" "cache_read"])
                                   (get-in model [:cost :cache-read])
                                   (get-in model [:cost :cache_read])))
   :cache-write (finite-number (or (get-in model ["cost" "cache_write"])
                                   (get-in model [:cost :cache-write])
                                   (get-in model [:cost :cache_write])))})

(defn- normalize-source-model
  [provider-name model-id model]
  (let [{:keys [provider api base-url]} (get supported-provider-rules provider-name)]
    {:id             model-id
     :name           (or (get model "name")
                         (get model :name)
                         model-id)
     :provider       provider
     :api            api
     :base-url       base-url
     :context-window (non-negative-int (or (get-in model ["limit" "context"])
                                           (get-in model [:limit :context]))
                                       4096)
     :max-tokens     (non-negative-int (or (get-in model ["limit" "output"])
                                           (get-in model [:limit :output]))
                                       4096)
     :cost           (normalize-cost model)
     :capabilities   {:reasoning? (true? (or (get model "reasoning")
                                             (get model :reasoning)))
                      :input      (supported-input model)}}))

(defn- model-key
  [model]
  [(:provider model) (:id model)])

(defn- assert-unique-model-keys!
  [models context]
  (let [duplicates (->> models
                        (map model-key)
                        frequencies
                        (filter (fn [[_ count]] (> count 1)))
                        (map first)
                        vec)]
    (when (seq duplicates)
      (throw (ex-info "Duplicate model key(s) detected"
                      {:context    context
                       :duplicates duplicates})))
    models))

(defn- extract-source-models
  [models-dev-data]
  (->> supported-provider-rules
       keys
       (mapcat
        (fn [provider-name]
          (let [provider-data (or (get models-dev-data provider-name)
                                  (get models-dev-data (keyword provider-name)))
                models        (or (get provider-data "models")
                                  (get provider-data :models)
                                  {})]
            (for [[model-id model] models
                  :let             [tool-call? (or (get model "tool_call")
                                                   (get model :tool_call))]
                  :when            (true? tool-call?)]
              (normalize-source-model provider-name (key->string model-id) model)))))
       vec))

(defn- deep-merge
  "Recursively merges maps. Non-map values from the right-hand side replace left-hand values."
  [& xs]
  (letfn [(merge-two [a b]
            (cond
              (and (map? a) (map? b)) (merge-with merge-two a b)
              :else b))]
    (reduce merge-two {} xs)))

(defn- normalize-model-after-merge
  [model]
  (if (contains? (get model :capabilities {}) :input)
    (update-in model [:capabilities :input] #(into (sorted-set) %))
    model))

(defn- apply-override-entry
  [models-by-key source-models [override-id raw-patch]]
  (let [override-id (key->string override-id)
        patch       (or raw-patch {})
        _           (when-not (map? patch)
                      (throw (ex-info "Override patch must be a map"
                                      {:id override-id :patch patch})))
        _           (when (contains? patch :id)
                      (throw (ex-info "Override patch must not include :id"
                                      {:id override-id :patch patch})))
        matched     (filterv #(= override-id (:id %)) source-models)]
    (case (count matched)
      0 (let [new-model (schema/assert-valid!
                         :llx/model
                         (normalize-model-after-merge
                          (deep-merge {} (assoc patch :id override-id))))]
          (assoc models-by-key (model-key new-model) new-model))

      1 (let [base-model (first matched)
              _          (when (and (contains? patch :provider)
                                    (not= (:provider patch) (:provider base-model)))
                           (throw (ex-info "Override patch cannot change provider for an existing model id"
                                           {:id   override-id
                                            :from (:provider base-model)
                                            :to   (:provider patch)})))
              merged     (schema/assert-valid!
                          :llx/model
                          (normalize-model-after-merge
                           (assoc (deep-merge base-model patch)
                                  :id override-id)))]
          (assoc models-by-key (model-key merged) merged))

      (throw (ex-info "Override id matches multiple source models; key by provider+id is required to disambiguate"
                      {:id      override-id
                       :matches (mapv model-key matched)})))))

(defn build-catalog
  "Builds canonical model entries from models.dev source data and local override patches.

  `overrides` is a map of `model-id -> patch-map`.
  Existing source models are patched in a fine-grained way (nested maps merged).
  If a model id is absent in source, the patch map must include a full valid model definition."
  [{:keys [models-dev-data overrides]}]
  (let [source-models (assert-unique-model-keys!
                       (->> (extract-source-models models-dev-data)
                            (mapv #(schema/assert-valid! :llx/model %)))
                       :source)
        _             (when-not (or (nil? overrides) (map? overrides))
                        (throw (ex-info "Overrides must be a map of model-id to patch map"
                                        {:overrides overrides})))
        merged-by-key (reduce (fn [acc entry]
                                (apply-override-entry acc source-models entry))
                              (into {} (map (juxt model-key identity) source-models))
                              (or overrides {}))]
    (->> (assert-unique-model-keys! (vals merged-by-key) :merged)
         (sort-by (juxt :provider :id))
         vec)))

(defn- catalog->provider-map
  [catalog]
  (reduce
   (fn [acc model]
     (update acc
             (:provider model)
             (fn [provider-models]
               (assoc (or provider-models (sorted-map))
                      (:id model)
                      model))))
   (sorted-map)
   catalog))

(defn render-generated-source
  "Renders deterministic source for `llx-ai.models-generated` from normalized catalog entries."
  [catalog]
  (let [provider-map (catalog->provider-map catalog)
        rendered-map (with-out-str
                       (binding [*print-namespace-maps* false]
                         (pprint/pprint provider-map)))]
    (str ";; This file is generated by `bb generate-models`.\n"
         ";; Do not edit manually.\n"
         "(ns llx-ai.models-generated)\n\n"
         "(def generated-models\n"
         rendered-map
         ")\n")))

(defn write-generated-artifact!
  [{:keys [path content]}]
  (spit path content)
  path)

(defn- load-override-models
  []
  (-> overrides-path io/file slurp edn/read-string))

(defn generate-models!
  [_opts]
  (let [models-dev-data (fetch-models-dev-data)
        overrides       (load-override-models)
        catalog         (build-catalog {:models-dev-data models-dev-data
                                        :overrides       overrides})
        rendered        (render-generated-source catalog)]
    (write-generated-artifact! {:path generated-artifact-path :content rendered})))

(defn -main
  [& _args]
  (generate-models! {})
  (println "models generated artifact updated"))
