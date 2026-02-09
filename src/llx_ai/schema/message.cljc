(ns llx-ai.schema.message)

(defn usage-total-valid?
  [{:keys [input output cache-read cache-write total-tokens]}]
  (= total-tokens (+ input output cache-read cache-write)))

(def schemas
  {:llx/content-text
   [:map {:closed true}
    [:type [:= :text]]
    [:text :string]]

   :llx/content-thinking
   [:map {:closed true}
    [:type [:= :thinking]]
    [:thinking :string]
    [:signature {:optional true} :string]]

   :llx/content-image
   [:map {:closed true}
    [:type [:= :image]]
    [:data :string]
    [:mime-type :string]]

   :llx/content-tool-call
   [:map {:closed true}
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
   [:map {:closed true}
    [:input :llx/non-neg-number]
    [:output :llx/non-neg-number]
    [:cache-read :llx/non-neg-number]
    [:cache-write :llx/non-neg-number]
    [:total :llx/non-neg-number]]

   :llx/usage
   [:and
    [:map {:closed true}
     [:input :llx/non-neg-int]
     [:output :llx/non-neg-int]
     [:cache-read :llx/non-neg-int]
     [:cache-write :llx/non-neg-int]
     [:total-tokens :llx/non-neg-int]
     [:cost :llx/usage-cost]]
    [:fn usage-total-valid?]]

   :llx/message-user
   [:map {:closed true}
    [:role [:= :user]]
    [:content [:or :string [:vector :llx/user-content-block]]]
    [:timestamp :llx/timestamp-ms]]

   :llx/message-assistant
   [:map {:closed true}
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
   [:map {:closed true}
    [:role [:= :tool-result]]
    [:tool-call-id :llx/id-string]
    [:tool-name :llx/id-string]
    [:content [:vector :llx/tool-result-content-block]]
    [:is-error? :boolean]
    [:timestamp :llx/timestamp-ms]]

   :llx/message
   [:or :llx/message-user :llx/message-assistant :llx/message-tool-result]

   :llx/context
   [:vector :llx/message]})
