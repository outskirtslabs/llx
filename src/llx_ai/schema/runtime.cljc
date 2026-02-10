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
    [:log/error {:optional true} :llx/fn]
    [:thread/sleep {:optional true} :llx/fn]]

   :llx/http-response-map
   [:map
    [:status {:optional true} :llx/non-neg-int]
    [:headers {:optional true} :map]
    [:body {:optional true} :any]]

   :llx/runtime-stream-state
   [:map
    [:model :llx/model]
    [:assistant-message {:optional true} :llx/message-assistant]]

   :llx/runtime-finalize-input
   [:or
    :llx/runtime-stream-state
    [:map
     [:model :llx/model]
     [:response :llx/http-response-map]
     [:assistant-message {:optional true} :llx/message-assistant]]]

   :llx/runtime-normalize-error-partial
   [:maybe
    [:map
     [:model {:optional true} :llx/model]
     [:assistant-message {:optional true} :llx/message-assistant]]]

   :llx/raw-stream-chunk
   [:or :string :map]

   :llx/event-stream-map
   [:map
    [:queue :any]
    [:closed? :any]
    [:is-complete? :llx/fn]
    [:extract-result :llx/fn]
    [:result* :any]]

   :llx/registry-entry
   [:map {:closed true}
    [:adapter :llx/adapter]
    [:source-id {:optional true} :any]]

   :llx/registry-map
   [:map
    [:llx.registry/adapters {:optional true} [:map-of :llx/api :llx/registry-entry]]
    [:llx.registry/tools {:optional true} :map]]

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
    [:state :llx/runtime-stream-state]
    [:events [:vector :llx/event]]]

   :llx/runtime-finalize-result
   [:map {:closed true}
    [:assistant-message :llx/message-assistant]
    [:events [:vector :llx/event]]]

   :llx/runtime-run-stream-input
   [:map
    [:adapter :llx/adapter]
    [:env :llx/env]
    [:model :llx/model]
    [:request :llx/adapter-request-map]
    [:out :map]
    [:state* :any]
    [:request-opts {:optional true} [:maybe :llx/request-options]]]

   :llx/runtime-run-stream-args
   :llx/runtime-run-stream-input})
