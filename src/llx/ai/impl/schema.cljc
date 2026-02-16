(ns llx.ai.impl.schema
  (:require
   [clojure.string :as str]
   [com.fulcrologic.guardrails.malli.registry :as gr.reg]
   [llx.ai.schema.runtime :as runtime]
   [malli.core :as m]
   [malli.error :as me]
   [malli.json-schema :as mjs]
   [promesa.core :as p]))

(defn non-blank-string?
  [s]
  (and (string? s) (not (str/blank? s))))

(defn non-negative-int?
  [n]
  (and (int? n) (<= 0 n)))

(defn non-negative-number?
  [n]
  (and (number? n) (<= 0 n)))

(defn deferred?
  [x]
  (p/deferred? x))

(defn schema-form?
  [x]
  (or (m/schema? x)
      (keyword? x)
      (vector? x)
      (map? x)
      (set? x)))

(def schemas
  {:llx/id-string
   [:and :string [:fn non-blank-string?]]

   :llx/non-neg-int
   [:fn non-negative-int?]

   :llx/non-neg-number
   [:fn non-negative-number?]

   :llx/provider
   [:enum :openai :anthropic :google :mistral :openai-compatible]

   :llx/api
   [:enum :openai-responses :openai-completions :anthropic-messages :google-generative-ai]

   :llx/role
   [:enum :user :assistant :tool-result]

   :llx/stop-reason
   [:enum :stop :length :tool-use :error :aborted]

   :llx/reasoning-level
   [:enum :minimal :low :medium :high :xhigh]

   :llx/thinking-budgets
   [:map
    [:minimal {:optional true} :llx/non-neg-int]
    [:low {:optional true} :llx/non-neg-int]
    [:medium {:optional true} :llx/non-neg-int]
    [:high {:optional true} :llx/non-neg-int]]

   :llx/cache-control
   [:enum :none :short :long]

   :llx/timestamp-ms
   :llx/non-neg-int

   :llx/fn
   [:fn ifn?]

   :llx/deferred
   [:fn deferred?]

   :llx/metadata-key
   [:or :keyword :string]

   :llx/metadata-map
   [:map-of :llx/metadata-key :any]

   :llx/cost-table
   [:map
    [:input :llx/non-neg-number]
    [:output :llx/non-neg-number]
    [:cache-read :llx/non-neg-number]
    [:cache-write :llx/non-neg-number]]

   :llx/model-capabilities
   [:map
    [:reasoning? :boolean]
    [:input [:set {:min 1} [:enum :text :image]]]
    [:supports-xhigh? {:optional true} :boolean]]

   :llx/model-compat
   [:map
    [:store? {:optional true} :boolean]
    [:supports-developer-role? {:optional true} :boolean]
    [:supports-strict-tools? {:optional true} :boolean]
    [:supports-usage-stream? {:optional true} :boolean]
    [:requires-tool-result-name? {:optional true} :boolean]
    [:requires-assistant-after-tool-result? {:optional true} :boolean]
    [:requires-thinking-as-text? {:optional true} :boolean]
    [:token-field {:optional true} [:enum :max_tokens :max_completion_tokens]]
    [:tool-result-role {:optional true} [:enum :tool :function]]
    [:thinking-format {:optional true} [:enum :reasoning_content :reasoning :text :openai :zai :qwen]]
    [:supports-reasoning-effort? {:optional true} :boolean]
    [:tool-id-format {:optional true} [:enum :any :mistral-9-alnum]]]

   :llx/model
   [:map
    [:id :llx/id-string]
    [:name :llx/id-string]
    [:provider :llx/provider]
    [:api :llx/api]
    [:base-url :llx/id-string]
    [:context-window :llx/non-neg-int]
    [:max-tokens :llx/non-neg-int]
    [:cost :llx/cost-table]
    [:capabilities :llx/model-capabilities]
    [:headers {:optional true} [:map-of :string :string]]
    [:compat {:optional true} :llx/model-compat]]

   :llx/content-text
   [:map
    [:type [:= :text]]
    [:text :string]]

   :llx/content-thinking
   [:map
    [:type [:= :thinking]]
    [:thinking :string]
    [:signature {:optional true} :string]]

   :llx/content-image
   [:map
    [:type [:= :image]]
    [:data :string]
    [:mime-type :string]]

   :llx/content-tool-call
   [:map
    [:type [:= :tool-call]]
    [:id :llx/id-string]
    [:name :llx/id-string]
    [:arguments :map]
    [:signature {:optional true} :string]]

   :llx/user-content-block
   [:or :llx/content-text :llx/content-image]

   :llx/assistant-content-block
   [:or :llx/content-text :llx/content-thinking :llx/content-tool-call]

   :llx/tool-result-content-block
   [:or :llx/content-text :llx/content-image]

   :llx/usage-cost
   [:map
    [:input :llx/non-neg-number]
    [:output :llx/non-neg-number]
    [:cache-read :llx/non-neg-number]
    [:cache-write :llx/non-neg-number]
    [:total :llx/non-neg-number]]

   :llx/usage
   [:map
    [:input :llx/non-neg-int]
    [:output :llx/non-neg-int]
    [:cache-read :llx/non-neg-int]
    [:cache-write :llx/non-neg-int]
    [:total-tokens :llx/non-neg-int]
    [:cost :llx/usage-cost]]

   :llx/message-user
   [:map
    [:role [:= :user]]
    [:content [:or :string [:vector :llx/user-content-block]]]
    [:timestamp :llx/timestamp-ms]]

   :llx/message-assistant
   [:map
    [:role [:= :assistant]]
    [:content [:vector :llx/assistant-content-block]]
    [:api :llx/api]
    [:provider :llx/provider]
    [:model :llx/id-string]
    [:usage :llx/usage]
    [:stop-reason :llx/stop-reason]
    [:error-message {:optional true} :string]
    [:timestamp :llx/timestamp-ms]]

   :llx/message-tool-result
   [:map
    [:role [:= :tool-result]]
    [:tool-call-id :llx/id-string]
    [:tool-name :llx/id-string]
    [:content [:vector :llx/tool-result-content-block]]
    [:is-error? :boolean]
    [:timestamp :llx/timestamp-ms]]

   :llx/message
   [:multi {:dispatch :role}
    [:user :llx/message-user]
    [:assistant :llx/message-assistant]
    [:tool-result :llx/message-tool-result]]

   :llx/messages
   [:vector :llx/message]

   :llx/context
   [:vector :llx/message]

   :llx/context-map
   [:map
    [:messages :llx/context]
    [:system-prompt {:optional true} :string]
    [:tools {:optional true} :llx/tools]]

   :llx/event-start
   [:map
    [:type [:= :start]]
    [:meta {:optional true} :map]]

   :llx/event-text-start
   [:map
    [:type [:= :text-start]]]

   :llx/event-text-delta
   [:map
    [:type [:= :text-delta]]
    [:text :string]]

   :llx/event-text-end
   [:map
    [:type [:= :text-end]]]

   :llx/event-thinking-start
   [:map
    [:type [:= :thinking-start]]]

   :llx/event-thinking-delta
   [:map
    [:type [:= :thinking-delta]]
    [:thinking :string]]

   :llx/event-thinking-end
   [:map
    [:type [:= :thinking-end]]]

   :llx/event-toolcall-start
   [:map
    [:type [:= :toolcall-start]]
    [:id :llx/id-string]
    [:name :llx/id-string]]

   :llx/event-toolcall-delta
   [:map
    [:type [:= :toolcall-delta]]
    [:id :llx/id-string]
    [:name :llx/id-string]
    [:arguments :map]]

   :llx/event-toolcall-end
   [:map
    [:type [:= :toolcall-end]]
    [:id :llx/id-string]
    [:name :llx/id-string]
    [:arguments :map]]

   :llx/event-done
   [:map
    [:type [:= :done]]
    [:assistant-message :llx/message-assistant]]

   :llx/event-error
   [:map
    [:type [:= :error]]
    [:assistant-message :llx/message-assistant]]

   :llx/event
   [:or
    :llx/event-start
    :llx/event-text-start
    :llx/event-text-delta
    :llx/event-text-end
    :llx/event-thinking-start
    :llx/event-thinking-delta
    :llx/event-thinking-end
    :llx/event-toolcall-start
    :llx/event-toolcall-delta
    :llx/event-toolcall-end
    :llx/event-done
    :llx/event-error]

   :llx/tool
   [:map
    [:name :llx/id-string]
    [:description :llx/id-string]
    [:input-schema [:fn schema-form?]]]

   :llx/tools
   [:vector :llx/tool]

   :llx/unified-request-options
   [:map
    [:max-tokens {:optional true} :llx/non-neg-int]
    [:reasoning {:optional true} :llx/reasoning-level]
    [:reasoning-effort {:optional true} :llx/reasoning-level]
    [:session-id {:optional true} :llx/id-string]
    [:temperature {:optional true} :llx/non-neg-number]
    [:top-p {:optional true} :llx/non-neg-number]
    [:thinking-budgets {:optional true} :llx/thinking-budgets]
    [:api-key {:optional true} :llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:signal {:optional true} :any]
    [:max-retry-delay-ms {:optional true} :llx/non-neg-int]
    [:metadata {:optional true} :llx/metadata-map]
    [:registry {:optional true} :any]]

   :llx/provider-reasoning-options
   [:map {:closed false}
    [:level {:optional true} :llx/reasoning-level]
    [:effort {:optional true} :llx/reasoning-level]
    [:summary {:optional true} [:or [:enum :auto :detailed :concise :none] :nil]]]

   :llx/provider-request-options
   [:map {:closed false}
    [:tools {:optional true} :llx/tools]
    [:tool-choice {:optional true} :any]
    [:reasoning {:optional true} :llx/provider-reasoning-options]
    [:cache-control {:optional true} :llx/cache-control]
    [:session-id {:optional true} :llx/id-string]
    [:api-key {:optional true} :llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:temperature {:optional true} :llx/non-neg-number]
    [:top-p {:optional true} :llx/non-neg-number]
    [:max-output-tokens {:optional true} :llx/non-neg-int]
    [:signal {:optional true} :any]
    [:max-retry-delay-ms {:optional true} :llx/non-neg-int]
    [:metadata {:optional true} :llx/metadata-map]
    [:registry {:optional true} :any]
    [:max-retries {:optional true} :llx/non-neg-int]]

   :llx/openai-completions-provider-options
   [:map {:closed false}
    [:tools {:optional true} :llx/tools]
    [:tool-choice {:optional true} [:or [:enum :auto :none :required] :llx/id-string]]
    [:reasoning {:optional true}
     [:map
      [:level {:optional true} :llx/reasoning-level]]]
    [:cache-control {:optional true} :llx/cache-control]
    [:session-id {:optional true} :llx/id-string]
    [:api-key {:optional true} :llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:temperature {:optional true} :llx/non-neg-number]
    [:top-p {:optional true} :llx/non-neg-number]
    [:max-output-tokens {:optional true} :llx/non-neg-int]
    [:signal {:optional true} :any]
    [:metadata {:optional true} :llx/metadata-map]
    [:registry {:optional true} :any]
    [:max-retries {:optional true} :llx/non-neg-int]]

   :llx/openai-responses-provider-options
   [:map {:closed false}
    [:tools {:optional true} :llx/tools]
    [:tool-choice {:optional true} [:or [:enum :auto :none] :llx/id-string]]
    [:reasoning {:optional true}
     [:map
      [:effort {:optional true} :llx/reasoning-level]
      [:summary {:optional true} [:or [:enum :auto :detailed :concise :none] :nil]]]]
    [:cache-control {:optional true} :llx/cache-control]
    [:session-id {:optional true} :llx/id-string]
    [:api-key {:optional true} :llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:temperature {:optional true} :llx/non-neg-number]
    [:top-p {:optional true} :llx/non-neg-number]
    [:max-output-tokens {:optional true} :llx/non-neg-int]
    [:signal {:optional true} :any]
    [:metadata {:optional true} :llx/metadata-map]
    [:registry {:optional true} :any]
    [:max-retries {:optional true} :llx/non-neg-int]]

   :llx/anthropic-provider-options
   [:map {:closed false}
    [:tools {:optional true} :llx/tools]
    [:tool-choice {:optional true} [:or [:enum :auto :none :any] :llx/id-string]]
    [:reasoning {:optional true}
     [:map
      [:level {:optional true} :llx/reasoning-level]]]
    [:cache-control {:optional true} :llx/cache-control]
    [:session-id {:optional true} :llx/id-string]
    [:api-key {:optional true} :llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:temperature {:optional true} :llx/non-neg-number]
    [:top-p {:optional true} :llx/non-neg-number]
    [:max-output-tokens {:optional true} :llx/non-neg-int]
    [:signal {:optional true} :any]
    [:metadata {:optional true} :llx/metadata-map]
    [:registry {:optional true} :any]
    [:max-retries {:optional true} :llx/non-neg-int]]

   :llx/google-provider-options
   [:map {:closed false}
    [:tools {:optional true} :llx/tools]
    [:tool-choice {:optional true} [:or [:enum :auto :none :any] :llx/id-string]]
    [:reasoning {:optional true}
     [:map
      [:level {:optional true} :llx/reasoning-level]
      [:effort {:optional true} :llx/reasoning-level]]]
    [:cache-control {:optional true} :llx/cache-control]
    [:session-id {:optional true} :llx/id-string]
    [:api-key {:optional true} :llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:temperature {:optional true} :llx/non-neg-number]
    [:top-p {:optional true} :llx/non-neg-number]
    [:max-output-tokens {:optional true} :llx/non-neg-int]
    [:signal {:optional true} :any]
    [:metadata {:optional true} :llx/metadata-map]
    [:registry {:optional true} :any]
    [:max-retries {:optional true} :llx/non-neg-int]]

   :llx/provider-config
   [:map
    [:api-key :llx/id-string]
    [:base-url {:optional true} :llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:organization {:optional true} :llx/id-string]
    [:project-id {:optional true} :llx/id-string]
    [:location {:optional true} :llx/id-string]]

   :llx/providers-config
   [:map
    [:openai {:optional true} :llx/provider-config]
    [:anthropic {:optional true} :llx/provider-config]
    [:google {:optional true} :llx/provider-config]
    [:mistral {:optional true} :llx/provider-config]
    [:openai-compatible {:optional true} :llx/provider-config]]

   :llx/library-config
   [:map
    [:providers :llx/providers-config]
    [:models {:optional true} [:vector :llx/model]]
    [:default-provider {:optional true} :llx/provider]]})

(defn custom-schemas
  []
  (merge schemas
         runtime/schemas))

(defn registry
  []
  (merge (m/default-schemas)
         (custom-schemas)))

(gr.reg/merge-schemas! (custom-schemas))

(defn schema
  [schema-id]
  (m/schema schema-id {:registry (registry)}))

(defn valid?
  [schema-id data]
  (m/validate (schema schema-id) data))

(defn explain
  [schema-id data]
  (m/explain (schema schema-id) data))

(defn humanize
  [schema-id data]
  (me/humanize (explain schema-id data)))

(defn assert-valid!
  [schema-id data]
  (if (valid? schema-id data)
    data
    (throw
     (ex-info "Schema validation failed"
              {:schema schema-id
               :errors (humanize schema-id data)
               :data   data}))))

(defn malli->json-schema
  "Converts a Malli schema form to a JSON Schema object map."
  [schema-form]
  (if (map? schema-form)
    schema-form
    (mjs/transform schema-form)))
