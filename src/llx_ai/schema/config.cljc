(ns llx-ai.schema.config)

(def schemas
  {:llx/provider-config
   [:map {:closed true}
    [:api-key :llx/id-string]
    [:base-url {:optional true} :llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:organization {:optional true} :llx/id-string]
    [:project-id {:optional true} :llx/id-string]
    [:location {:optional true} :llx/id-string]]

   :llx/providers-config
   [:map {:closed true}
    [:openai {:optional true} :llx/provider-config]
    [:anthropic {:optional true} :llx/provider-config]
    [:google {:optional true} :llx/provider-config]
    [:mistral {:optional true} :llx/provider-config]
    [:openai-compatible {:optional true} :llx/provider-config]]

   :llx/library-config
   [:map {:closed true}
    [:providers :llx/providers-config]
    [:models {:optional true} [:vector :llx/model]]
    [:default-provider {:optional true} :llx/provider]]})
