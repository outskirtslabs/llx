(ns llx.ai.impl.schema.options
  (:require
   [malli.core :as m]))

(defn schema-form?
  [x]
  (or (m/schema? x)
      (keyword? x)
      (vector? x)
      (map? x)
      (set? x)))

(def schemas
  {:llx/tool
   [:map {:closed true}
    [:name :llx/id-string]
    [:description :llx/id-string]
    [:input-schema [:fn schema-form?]]]

   :llx/unified-request-options
   [:map {:closed true}
    [:max-tokens {:optional true} :llx/non-neg-int]
    [:reasoning {:optional true} :llx/reasoning-level]
    [:reasoning-effort {:optional true} :llx/reasoning-level]
    [:temperature {:optional true} :llx/non-neg-number]
    [:top-p {:optional true} :llx/non-neg-number]
    [:api-key {:optional true} :llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:signal {:optional true} :any]
    [:metadata {:optional true} :llx/metadata-map]
    [:registry {:optional true} :any]]

   :llx/provider-reasoning-options
   [:map {:closed false}
    [:level {:optional true} :llx/reasoning-level]
    [:effort {:optional true} :llx/reasoning-level]
    [:summary {:optional true} [:or [:enum :auto :detailed :concise :none] :nil]]]

   :llx/provider-request-options
   [:map {:closed false}
    [:tools {:optional true} [:vector :llx/tool]]
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
    [:metadata {:optional true} :llx/metadata-map]
    [:registry {:optional true} :any]
    [:max-retries {:optional true} :llx/non-neg-int]]

   :llx/openai-completions-provider-options
   [:map {:closed false}
    [:tools {:optional true} [:vector :llx/tool]]
    [:tool-choice {:optional true} [:or [:enum :auto :none :required] :llx/id-string]]
    [:reasoning {:optional true}
     [:map {:closed true}
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
    [:tools {:optional true} [:vector :llx/tool]]
    [:tool-choice {:optional true} [:or [:enum :auto :none] :llx/id-string]]
    [:reasoning {:optional true}
     [:map {:closed true}
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
    [:tools {:optional true} [:vector :llx/tool]]
    [:tool-choice {:optional true} [:or [:enum :auto :none :any] :llx/id-string]]
    [:reasoning {:optional true}
     [:map {:closed true}
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
    [:tools {:optional true} [:vector :llx/tool]]
    [:tool-choice {:optional true} [:or [:enum :auto :none :any] :llx/id-string]]
    [:reasoning {:optional true}
     [:map {:closed true}
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
    [:max-retries {:optional true} :llx/non-neg-int]]})
