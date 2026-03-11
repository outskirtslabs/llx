(ns ol.llx.ai.impl.schema
  (:require
   [clojure.string :as str]
   [com.fulcrologic.guardrails.malli.registry :as gr.reg]
   [ol.llx.ai.schema.runtime :as runtime]
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
  {:ol.llx/id-string
   [:and :string [:fn non-blank-string?]]

   :ol.llx/non-neg-int
   [:fn non-negative-int?]

   :ol.llx/non-neg-number
   [:fn non-negative-number?]

   :ol.llx/provider
   [:enum :openai :openai-codex :anthropic :google :mistral :openai-compatible]

   :ol.llx/api
   [:enum :openai-responses :openai-codex-responses :openai-completions :anthropic-messages :google-generative-ai]

   :ol.llx/role
   [:enum :user :assistant :tool-result]

   :ol.llx/stop-reason
   [:enum :stop :length :tool-use :error :aborted]

   :ol.llx/reasoning-level
   [:enum :minimal :low :medium :high :xhigh]

   :ol.llx/thinking-budgets
   [:map
    [:minimal {:optional true} :ol.llx/non-neg-int]
    [:low {:optional true} :ol.llx/non-neg-int]
    [:medium {:optional true} :ol.llx/non-neg-int]
    [:high {:optional true} :ol.llx/non-neg-int]]

   :ol.llx/cache-control
   [:enum :none :short :long]

   :ol.llx/timestamp-ms
   :ol.llx/non-neg-int

   :ol.llx/fn
   [:fn ifn?]

   :ol.llx/deferred
   [:fn deferred?]

   :ol.llx/metadata-key
   [:or :keyword :string]

   :ol.llx/metadata-map
   [:map-of :ol.llx/metadata-key :any]

   :ol.llx/cost-table
   [:map
    [:input :ol.llx/non-neg-number]
    [:output :ol.llx/non-neg-number]
    [:cache-read :ol.llx/non-neg-number]
    [:cache-write :ol.llx/non-neg-number]]

   :ol.llx/model-capabilities
   [:map
    [:reasoning? :boolean]
    [:input [:set {:min 1} [:enum :text :image]]]
    [:supports-xhigh? {:optional true} :boolean]]

   :ol.llx/model-compat
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

   :ol.llx/model
   [:map
    [:id :ol.llx/id-string]
    [:name :ol.llx/id-string]
    [:provider :ol.llx/provider]
    [:api :ol.llx/api]
    [:base-url :ol.llx/id-string]
    [:context-window :ol.llx/non-neg-int]
    [:max-tokens :ol.llx/non-neg-int]
    [:cost :ol.llx/cost-table]
    [:capabilities :ol.llx/model-capabilities]
    [:headers {:optional true} [:map-of :string :string]]
    [:compat {:optional true} :ol.llx/model-compat]]

   :ol.llx/content-text
   [:map
    [:type [:= :text]]
    [:text :string]
    [:signature {:optional true} :string]]

   :ol.llx/content-thinking
   [:map
    [:type [:= :thinking]]
    [:thinking :string]
    [:redacted {:optional true} :boolean]
    [:signature {:optional true} :string]]

   :ol.llx/content-image
   [:map
    [:type [:= :image]]
    [:data :string]
    [:mime-type :string]]

   :ol.llx/content-tool-call
   [:map
    [:type [:= :tool-call]]
    [:id :ol.llx/id-string]
    [:name :ol.llx/id-string]
    [:arguments :map]
    [:signature {:optional true} :string]]

   :ol.llx/user-content-block
   [:or :ol.llx/content-text :ol.llx/content-image]

   :ol.llx/assistant-content-block
   [:or :ol.llx/content-text :ol.llx/content-thinking :ol.llx/content-tool-call]

   :ol.llx/tool-result-content-block
   [:or :ol.llx/content-text :ol.llx/content-image]

   :ol.llx/usage-cost
   [:map
    [:input :ol.llx/non-neg-number]
    [:output :ol.llx/non-neg-number]
    [:cache-read :ol.llx/non-neg-number]
    [:cache-write :ol.llx/non-neg-number]
    [:total :ol.llx/non-neg-number]]

   :ol.llx/usage
   [:map
    [:input :ol.llx/non-neg-int]
    [:output :ol.llx/non-neg-int]
    [:cache-read :ol.llx/non-neg-int]
    [:cache-write :ol.llx/non-neg-int]
    [:total-tokens :ol.llx/non-neg-int]
    [:cost :ol.llx/usage-cost]]

   :ol.llx/message-user
   [:map
    [:role [:= :user]]
    [:content [:or :string [:vector :ol.llx/user-content-block]]]
    [:timestamp :ol.llx/timestamp-ms]]

   :ol.llx/message-assistant
   [:map
    [:role [:= :assistant]]
    [:content [:vector :ol.llx/assistant-content-block]]
    [:api :ol.llx/api]
    [:provider :ol.llx/provider]
    [:model :ol.llx/id-string]
    [:usage :ol.llx/usage]
    [:stop-reason :ol.llx/stop-reason]
    [:error-message {:optional true} :string]
    [:timestamp :ol.llx/timestamp-ms]]

   :ol.llx/message-tool-result
   [:map
    [:role [:= :tool-result]]
    [:tool-call-id :ol.llx/id-string]
    [:tool-name :ol.llx/id-string]
    [:content [:vector :ol.llx/tool-result-content-block]]
    [:is-error? :boolean]
    [:timestamp :ol.llx/timestamp-ms]]

   :ol.llx/message
   [:multi {:dispatch :role}
    [:user :ol.llx/message-user]
    [:assistant :ol.llx/message-assistant]
    [:tool-result :ol.llx/message-tool-result]]

   :ol.llx/messages
   [:vector :ol.llx/message]

   :ol.llx/context
   [:vector :ol.llx/message]

   :ol.llx/context-map
   [:map
    [:messages :ol.llx/context]
    [:system-prompt {:optional true} :string]
    [:tools {:optional true} :ol.llx/tools]]

   :ol.llx/event-start
   [:map
    [:type [:= :start]]
    [:meta {:optional true} :map]]

   :ol.llx/event-text-start
   [:map
    [:type [:= :text-start]]]

   :ol.llx/event-text-delta
   [:map
    [:type [:= :text-delta]]
    [:text :string]]

   :ol.llx/event-text-end
   [:map
    [:type [:= :text-end]]]

   :ol.llx/event-thinking-start
   [:map
    [:type [:= :thinking-start]]]

   :ol.llx/event-thinking-delta
   [:map
    [:type [:= :thinking-delta]]
    [:thinking :string]]

   :ol.llx/event-thinking-end
   [:map
    [:type [:= :thinking-end]]]

   :ol.llx/event-toolcall-start
   [:map
    [:type [:= :toolcall-start]]
    [:id :ol.llx/id-string]
    [:name :ol.llx/id-string]]

   :ol.llx/event-toolcall-delta
   [:map
    [:type [:= :toolcall-delta]]
    [:id :ol.llx/id-string]
    [:name :ol.llx/id-string]
    [:arguments :map]]

   :ol.llx/event-toolcall-end
   [:map
    [:type [:= :toolcall-end]]
    [:id :ol.llx/id-string]
    [:name :ol.llx/id-string]
    [:arguments :map]]

   :ol.llx/event-done
   [:map
    [:type [:= :done]]
    [:assistant-message :ol.llx/message-assistant]]

   :ol.llx/event-error
   [:map
    [:type [:= :error]]
    [:assistant-message :ol.llx/message-assistant]]

   :ol.llx/event
   [:or
    :ol.llx/event-start
    :ol.llx/event-text-start
    :ol.llx/event-text-delta
    :ol.llx/event-text-end
    :ol.llx/event-thinking-start
    :ol.llx/event-thinking-delta
    :ol.llx/event-thinking-end
    :ol.llx/event-toolcall-start
    :ol.llx/event-toolcall-delta
    :ol.llx/event-toolcall-end
    :ol.llx/event-done
    :ol.llx/event-error]

   :ol.llx/tool
   [:map
    [:name :ol.llx/id-string]
    [:description :ol.llx/id-string]
    [:input-schema [:fn schema-form?]]]

   :ol.llx/tools
   [:vector :ol.llx/tool]

   :ol.llx/oauth-provider-id
   :ol.llx/id-string

   :ol.llx/oauth-credentials
   [:map {:closed false}
    [:refresh :ol.llx/id-string]
    [:access :ol.llx/id-string]
    [:expires :ol.llx/non-neg-int]]

   :ol.llx/oauth-credentials-by-provider
   [:map-of :ol.llx/oauth-provider-id :ol.llx/oauth-credentials]

   :ol.llx/oauth-provider
   [:map {:closed false}
    [:id :ol.llx/oauth-provider-id]
    [:name :ol.llx/id-string]
    [:login :ol.llx/fn]
    [:refresh-token :ol.llx/fn]
    [:get-api-key :ol.llx/fn]
    [:uses-callback-server? {:optional true} :boolean]
    [:modify-models {:optional true} :ol.llx/fn]]

   :ol.llx/oauth-auth-info
   [:map
    [:url :ol.llx/id-string]
    [:instructions {:optional true} :string]]

   :ol.llx/oauth-prompt
   [:map
    [:message :ol.llx/id-string]
    [:placeholder {:optional true} :string]
    [:allow-empty? {:optional true} :boolean]]

   :ol.llx/oauth-login-callbacks
   [:map {:closed false}
    [:on-auth {:optional true} :ol.llx/fn]
    [:on-prompt {:optional true} :ol.llx/fn]
    [:on-progress {:optional true} :ol.llx/fn]
    [:on-manual-code-input {:optional true} :ol.llx/fn]
    [:signal {:optional true} :any]]

   :ol.llx/oauth-login-hooks
   [:map {:closed false}
    [:create-authorization-flow :ol.llx/fn]
    [:start-local-oauth-server :ol.llx/fn]
    [:exchange-authorization-code :ol.llx/fn]
    [:account-id-from-access-token :ol.llx/fn]
    [:redirect-uri {:optional true} :ol.llx/id-string]]

   :ol.llx/unified-request-options
   [:map
    [:max-tokens {:optional true} :ol.llx/non-neg-int]
    [:reasoning {:optional true} :ol.llx/reasoning-level]
    [:reasoning-effort {:optional true} :ol.llx/reasoning-level]
    [:session-id {:optional true} :ol.llx/id-string]
    [:temperature {:optional true} :ol.llx/non-neg-number]
    [:top-p {:optional true} :ol.llx/non-neg-number]
    [:thinking-budgets {:optional true} :ol.llx/thinking-budgets]
    [:api-key {:optional true} :ol.llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:signal {:optional true} :any]
    [:max-retry-delay-ms {:optional true} :ol.llx/non-neg-int]
    [:metadata {:optional true} :ol.llx/metadata-map]
    [:registry {:optional true} :any]]

   :ol.llx/provider-reasoning-options
   [:map {:closed false}
    [:level {:optional true} :ol.llx/reasoning-level]
    [:effort {:optional true} :ol.llx/reasoning-level]
    [:summary {:optional true} [:or [:enum :auto :detailed :concise :none] :nil]]]

   :ol.llx/provider-request-options
   [:map {:closed false}
    [:tools {:optional true} :ol.llx/tools]
    [:tool-choice {:optional true} :any]
    [:reasoning {:optional true} :ol.llx/provider-reasoning-options]
    [:cache-control {:optional true} :ol.llx/cache-control]
    [:session-id {:optional true} :ol.llx/id-string]
    [:api-key {:optional true} :ol.llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:temperature {:optional true} :ol.llx/non-neg-number]
    [:top-p {:optional true} :ol.llx/non-neg-number]
    [:max-output-tokens {:optional true} :ol.llx/non-neg-int]
    [:signal {:optional true} :any]
    [:max-retry-delay-ms {:optional true} :ol.llx/non-neg-int]
    [:metadata {:optional true} :ol.llx/metadata-map]
    [:registry {:optional true} :any]
    [:max-retries {:optional true} :ol.llx/non-neg-int]]

   :ol.llx/openai-completions-provider-options
   [:map {:closed false}
    [:tools {:optional true} :ol.llx/tools]
    [:tool-choice {:optional true} [:or [:enum :auto :none :required] :ol.llx/id-string]]
    [:reasoning {:optional true}
     [:map
      [:level {:optional true} :ol.llx/reasoning-level]]]
    [:cache-control {:optional true} :ol.llx/cache-control]
    [:session-id {:optional true} :ol.llx/id-string]
    [:api-key {:optional true} :ol.llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:temperature {:optional true} :ol.llx/non-neg-number]
    [:top-p {:optional true} :ol.llx/non-neg-number]
    [:max-output-tokens {:optional true} :ol.llx/non-neg-int]
    [:signal {:optional true} :any]
    [:metadata {:optional true} :ol.llx/metadata-map]
    [:registry {:optional true} :any]
    [:max-retries {:optional true} :ol.llx/non-neg-int]]

   :ol.llx/openai-responses-provider-options
   [:map {:closed false}
    [:tools {:optional true} :ol.llx/tools]
    [:tool-choice {:optional true} [:or [:enum :auto :none] :ol.llx/id-string]]
    [:reasoning {:optional true}
     [:map
      [:effort {:optional true} :ol.llx/reasoning-level]
      [:summary {:optional true} [:or [:enum :auto :detailed :concise :none] :nil]]]]
    [:cache-control {:optional true} :ol.llx/cache-control]
    [:session-id {:optional true} :ol.llx/id-string]
    [:api-key {:optional true} :ol.llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:temperature {:optional true} :ol.llx/non-neg-number]
    [:top-p {:optional true} :ol.llx/non-neg-number]
    [:max-output-tokens {:optional true} :ol.llx/non-neg-int]
    [:signal {:optional true} :any]
    [:metadata {:optional true} :ol.llx/metadata-map]
    [:registry {:optional true} :any]
    [:max-retries {:optional true} :ol.llx/non-neg-int]]

   :ol.llx/openai-codex-responses-provider-options
   [:map {:closed false}
    [:tools {:optional true} :ol.llx/tools]
    [:tool-choice {:optional true} [:or [:enum :auto :none] :ol.llx/id-string]]
    [:reasoning {:optional true}
     [:map
      [:effort {:optional true} :ol.llx/reasoning-level]
      [:summary {:optional true} [:or [:enum :auto :detailed :concise :none] :nil]]]]
    [:cache-control {:optional true} :ol.llx/cache-control]
    [:session-id {:optional true} :ol.llx/id-string]
    [:api-key {:optional true} :ol.llx/id-string]
    [:originator {:optional true} :ol.llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:temperature {:optional true} :ol.llx/non-neg-number]
    [:top-p {:optional true} :ol.llx/non-neg-number]
    [:max-output-tokens {:optional true} :ol.llx/non-neg-int]
    [:signal {:optional true} :any]
    [:metadata {:optional true} :ol.llx/metadata-map]
    [:registry {:optional true} :any]
    [:max-retries {:optional true} :ol.llx/non-neg-int]]

   :ol.llx/anthropic-provider-options
   [:map {:closed false}
    [:tools {:optional true} :ol.llx/tools]
    [:tool-choice {:optional true} [:or [:enum :auto :none :any] :ol.llx/id-string]]
    [:reasoning {:optional true}
     [:map
      [:level {:optional true} :ol.llx/reasoning-level]]]
    [:cache-control {:optional true} :ol.llx/cache-control]
    [:session-id {:optional true} :ol.llx/id-string]
    [:api-key {:optional true} :ol.llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:temperature {:optional true} :ol.llx/non-neg-number]
    [:top-p {:optional true} :ol.llx/non-neg-number]
    [:max-output-tokens {:optional true} :ol.llx/non-neg-int]
    [:signal {:optional true} :any]
    [:metadata {:optional true} :ol.llx/metadata-map]
    [:registry {:optional true} :any]
    [:max-retries {:optional true} :ol.llx/non-neg-int]]

   :ol.llx/google-provider-options
   [:map {:closed false}
    [:tools {:optional true} :ol.llx/tools]
    [:tool-choice {:optional true} [:or [:enum :auto :none :any] :ol.llx/id-string]]
    [:reasoning {:optional true}
     [:map
      [:level {:optional true} :ol.llx/reasoning-level]
      [:effort {:optional true} :ol.llx/reasoning-level]]]
    [:cache-control {:optional true} :ol.llx/cache-control]
    [:session-id {:optional true} :ol.llx/id-string]
    [:api-key {:optional true} :ol.llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:temperature {:optional true} :ol.llx/non-neg-number]
    [:top-p {:optional true} :ol.llx/non-neg-number]
    [:max-output-tokens {:optional true} :ol.llx/non-neg-int]
    [:signal {:optional true} :any]
    [:metadata {:optional true} :ol.llx/metadata-map]
    [:registry {:optional true} :any]
    [:max-retries {:optional true} :ol.llx/non-neg-int]]

   :ol.llx/provider-config
   [:map
    [:api-key :ol.llx/id-string]
    [:base-url {:optional true} :ol.llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:organization {:optional true} :ol.llx/id-string]
    [:project-id {:optional true} :ol.llx/id-string]
    [:location {:optional true} :ol.llx/id-string]]

   :ol.llx/providers-config
   [:map
    [:openai {:optional true} :ol.llx/provider-config]
    [:openai-codex {:optional true} :ol.llx/provider-config]
    [:anthropic {:optional true} :ol.llx/provider-config]
    [:google {:optional true} :ol.llx/provider-config]
    [:mistral {:optional true} :ol.llx/provider-config]
    [:openai-compatible {:optional true} :ol.llx/provider-config]]

   :ol.llx/library-config
   [:map
    [:providers :ol.llx/providers-config]
    [:models {:optional true} [:vector :ol.llx/model]]
    [:default-provider {:optional true} :ol.llx/provider]]})

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
