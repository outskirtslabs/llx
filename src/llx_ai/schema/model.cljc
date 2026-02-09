(ns llx-ai.schema.model)

(def schemas
  {:llx/cost-table
   [:map {:closed true}
    [:input :llx/non-neg-number]
    [:output :llx/non-neg-number]
    [:cache-read :llx/non-neg-number]
    [:cache-write :llx/non-neg-number]]

   :llx/model-capabilities
   [:map {:closed true}
    [:reasoning? :boolean]
    [:input [:set {:min 1} [:enum :text :image]]]
    [:supports-xhigh? {:optional true} :boolean]]

   :llx/model-compat
   [:map {:closed true}
    [:store? {:optional true} :boolean]
    [:supports-developer-role? {:optional true} :boolean]
    [:supports-strict-tools? {:optional true} :boolean]
    [:supports-usage-stream? {:optional true} :boolean]
    [:token-field {:optional true} [:enum :max_tokens :max_completion_tokens]]
    [:tool-result-role {:optional true} [:enum :tool :function]]
    [:thinking-format {:optional true} [:enum :reasoning_content :reasoning :text]]
    [:tool-id-format {:optional true} [:enum :any :mistral-9-alnum]]]

   :llx/model
   [:map {:closed true}
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
    [:compat {:optional true} :llx/model-compat]]})
