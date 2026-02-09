(ns llx-ai.schema.runtime)

(def schemas
  {:llx/adapter
   [:map {:closed true}
    [:api :llx/api]
    [:build-request :llx/fn]
    [:open-stream :llx/fn]
    [:decode-event :llx/fn]
    [:finalize :llx/fn]
    [:normalize-error :llx/fn]
    [:supports-model? {:optional true} :llx/fn]
    [:normalize-tool-call-id {:optional true} :llx/fn]
    [:transform-options {:optional true} :map]
    [:transform-context {:optional true} :llx/fn]]

   :llx/env
   [:map {:closed true}
    [:http/request :llx/fn]
    [:json/encode :llx/fn]
    [:json/decode :llx/fn]
    [:json/decode-safe {:optional true} :llx/fn]
    [:http/read-body-string {:optional true} :llx/fn]
    [:stream/run! {:optional true} :llx/fn]
    [:registry {:optional true} :any]
    [:clock/now-ms :llx/fn]
    [:id/new :llx/fn]
    [:env/get {:optional true} :llx/fn]
    [:log/debug {:optional true} :llx/fn]
    [:log/warn {:optional true} :llx/fn]
    [:log/error {:optional true} :llx/fn]]

   :llx/adapter-request-map
   [:map
    [:method :keyword]
    [:url :string]
    [:headers {:optional true} [:map-of :string :string]]
    [:body {:optional true} :any]
    [:as {:optional true} :any]
    [:throw {:optional true} :boolean]]

   :llx/runtime-decode-event-result
   [:map {:closed true}
    [:state :map]
    [:events [:vector :llx/event]]]

   :llx/runtime-finalize-result
   [:map {:closed true}
    [:assistant-message :llx/message-assistant]
    [:events [:vector :llx/event]]]

   :llx/runtime-run-stream-args
   [:map {:closed true}
    [:adapter :llx/adapter]
    [:env :llx/env]
    [:model :llx/model]
    [:request :llx/adapter-request-map]
    [:out :map]
    [:state* :any]]})
