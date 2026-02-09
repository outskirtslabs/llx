(ns llx-ai.models
  (:require
   [clojure.string :as str]
   [llx-ai.schema :as schema]))

(def ^:private default-models
  {:openai
   {"gpt-4o-mini"
    {:id             "gpt-4o-mini"
     :name           "GPT-4o Mini"
     :provider       :openai
     :api            :openai-completions
     :base-url       "https://api.openai.com/v1"
     :context-window 128000
     :max-tokens     16384
     :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
     :capabilities   {:reasoning? false :input #{:text :image}}}
    "gpt-5-mini"
    {:id             "gpt-5-mini"
     :name           "GPT-5 Mini"
     :provider       :openai
     :api            :openai-responses
     :base-url       "https://api.openai.com/v1"
     :context-window 272000
     :max-tokens     65536
     :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
     :capabilities   {:reasoning? true :input #{:text :image}}}}

   :anthropic
   {"claude-sonnet-4-5"
    {:id             "claude-sonnet-4-5"
     :name           "Claude Sonnet 4.5"
     :provider       :anthropic
     :api            :anthropic-messages
     :base-url       "https://api.anthropic.com/v1"
     :context-window 200000
     :max-tokens     8192
     :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
     :capabilities   {:reasoning? true :input #{:text :image}}}}

   :google
   {"gemini-2.5-flash"
    {:id             "gemini-2.5-flash"
     :name           "Gemini 2.5 Flash"
     :provider       :google
     :api            :google-generative-ai
     :base-url       "https://generativelanguage.googleapis.com/v1beta"
     :context-window 1048576
     :max-tokens     65536
     :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
     :capabilities   {:reasoning? true :input #{:text :image}}}}

   :mistral
   {"devstral-medium-latest"
    {:id             "devstral-medium-latest"
     :name           "Devstral Medium"
     :provider       :mistral
     :api            :openai-completions
     :base-url       "https://api.mistral.ai/v1"
     :context-window 128000
     :max-tokens     8192
     :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
     :capabilities   {:reasoning? false :input #{:text :image}}
     :compat         {:token-field :max_tokens :tool-id-format :mistral-9-alnum}}}

   :openai-compatible
   {"llama3.2"
    {:id             "llama3.2"
     :name           "Llama 3.2"
     :provider       :openai-compatible
     :api            :openai-completions
     :base-url       "http://localhost:11434/v1"
     :context-window 8192
     :max-tokens     8192
     :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
     :capabilities   {:reasoning? false :input #{:text}}}}})

(defn get-model
  "Returns a model map for `provider` and `model-id`, or `nil` when not found."
  [provider model-id]
  (get-in default-models [provider (str model-id)]))

(defn get-providers
  "Returns supported providers as a deterministic sorted vector."
  []
  (->> (keys default-models)
       sort
       vec))

(defn get-models
  "Returns all models for `provider` as a deterministic vector sorted by `:id`."
  [provider]
  (->> (vals (get default-models provider {}))
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

(doseq [[provider models] default-models
        [_ model]         models]
  (when-not (schema/valid? :llx/model model)
    (throw (ex-info "Invalid built-in model" {:provider provider :model model}))))
