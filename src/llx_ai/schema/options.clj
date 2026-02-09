(ns llx-ai.schema.options
  (:require
   [malli.core :as m]))

(defn schema-form?
  [x]
  (try
    (m/schema x)
    true
    (catch Exception _
      false)))

(def schemas
  {:llx/reasoning-options
   [:map {:closed true}
    [:level :llx/reasoning-level]
    [:effort {:optional true} [:enum :minimal :low :medium :high]]
    [:summary {:optional true} [:enum :auto :detailed :none]]]

   :llx/tool
   [:map {:closed true}
    [:name :llx/id-string]
    [:description :llx/id-string]
    [:input-schema [:fn schema-form?]]]

   :llx/request-options
   [:map {:closed true}
    [:tools {:optional true} [:vector :llx/tool]]
    [:tool-choice {:optional true} [:or [:enum :auto :none] :llx/id-string]]
    [:reasoning {:optional true} :llx/reasoning-options]
    [:cache-control {:optional true} :llx/cache-control]
    [:api-key {:optional true} :llx/id-string]
    [:headers {:optional true} [:map-of :string :string]]
    [:temperature {:optional true} :llx/non-neg-number]
    [:top-p {:optional true} :llx/non-neg-number]
    [:max-output-tokens {:optional true} :llx/non-neg-int]
    [:signal {:optional true} :any]
    [:metadata {:optional true} :llx/metadata-map]]})
