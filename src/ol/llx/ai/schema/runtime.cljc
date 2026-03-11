(ns ol.llx.ai.schema.runtime
  (:require
   [promesa.exec.csp :as sp]))

(defn stream-channel?
  [x]
  (sp/chan? x))

(def schemas
  {:ol.llx/adapter
   [:map {:closed true}
    [:api :ol.llx/api]
    [:build-request :ol.llx/fn]
    [:open-stream :ol.llx/fn]
    [:decode-event :ol.llx/fn]
    [:finalize :ol.llx/fn]
    [:normalize-error :ol.llx/fn]
    [:supports-model? {:optional true} :ol.llx/fn]
    [:normalize-tool-call-id {:optional true} :ol.llx/fn]
    [:transform-options {:optional true} :map]
    [:transform-context {:optional true} :ol.llx/fn]]

   :ol.llx/env
   [:map {:closed true}
    [:http/request :ol.llx/fn]
    [:json/encode :ol.llx/fn]
    [:json/decode :ol.llx/fn]
    [:json/decode-safe {:optional true} :ol.llx/fn]
    [:http/read-body-string {:optional true} :ol.llx/fn]
    [:stream/run! {:optional true} :ol.llx/fn]
    [:registry {:optional true} :any]
    [:clock/now-ms :ol.llx/fn]
    [:id/new :ol.llx/fn]
    [:env/get {:optional true} :ol.llx/fn]
    [:log/debug {:optional true} :ol.llx/fn]
    [:log/warn {:optional true} :ol.llx/fn]
    [:log/error {:optional true} :ol.llx/fn]
    [:call/id {:optional true} :ol.llx/id-string]
    [:unicode/sanitize-payload :ol.llx/fn]]

   :ol.llx/http-response-map
   [:map
    [:status {:optional true} :ol.llx/non-neg-int]
    [:headers {:optional true} :map]
    [:body {:optional true} :any]]

   :ol.llx/runtime-stream-state
   [:map
    [:model :ol.llx/model]
    [:assistant-message {:optional true} :ol.llx/message-assistant]]

   :ol.llx/runtime-finalize-input
   [:or
    :ol.llx/runtime-stream-state
    [:map
     [:model :ol.llx/model]
     [:response :ol.llx/http-response-map]
     [:assistant-message {:optional true} :ol.llx/message-assistant]]]

   :ol.llx/runtime-normalize-error-partial
   [:maybe
    [:map
     [:model {:optional true} :ol.llx/model]
     [:assistant-message {:optional true} :ol.llx/message-assistant]]]

   :ol.llx/raw-stream-chunk
   [:or :string :map]

   :ol.llx/stream-channel
   [:fn stream-channel?]

   :ol.llx/registry-entry
   [:map {:closed true}
    [:adapter :ol.llx/adapter]
    [:source-id {:optional true} :any]]

   :ol.llx/registry-map
   [:map
    [:ol.llx.registry/adapters {:optional true} [:map-of :ol.llx/api :ol.llx/registry-entry]]
    [:ol.llx.registry/tools {:optional true} :map]]

   :ol.llx/adapter-request-map
   [:map
    [:method :keyword]
    [:url :string]
    [:headers {:optional true} [:map-of :string :string]]
    [:body {:optional true} :any]
    [:as {:optional true} :any]
    [:throw {:optional true} :boolean]]

   :ol.llx/runtime-decode-event-result
   [:map {:closed true}
    [:state :ol.llx/runtime-stream-state]
    [:events [:vector :ol.llx/event]]]

   :ol.llx/runtime-finalize-result
   [:map {:closed true}
    [:assistant-message :ol.llx/message-assistant]
    [:events [:vector :ol.llx/event]]]

   :ol.llx/runtime-start-source-input
   [:map {:closed true}
    [:adapter :ol.llx/adapter]
    [:env :ol.llx/env]
    [:model :ol.llx/model]
    [:request :ol.llx/adapter-request-map]
    [:response :ol.llx/http-response-map]
    [:payload-ch :ol.llx/stream-channel]
    [:control-ch :ol.llx/stream-channel]
    [:cancelled? :ol.llx/fn]]

   :ol.llx/runtime-start-source-result
   [:maybe
    [:map {:closed true}
     [:stop-fn {:optional true} :ol.llx/fn]]]

   :ol.llx/runtime-run-stream-base-input
   [:map
    [:adapter :ol.llx/adapter]
    [:env :ol.llx/env]
    [:model :ol.llx/model]
    [:request :ol.llx/adapter-request-map]
    [:out :ol.llx/stream-channel]
    [:state* :any]
    [:request-opts {:optional true} [:maybe :ol.llx/provider-request-options]]]

   :ol.llx/runtime-run-stream-input
   [:map
    [:adapter :ol.llx/adapter]
    [:env :ol.llx/env]
    [:model :ol.llx/model]
    [:request :ol.llx/adapter-request-map]
    [:out :ol.llx/stream-channel]
    [:state* :any]
    [:request-opts {:optional true} [:maybe :ol.llx/provider-request-options]]
    [:cancel! {:optional true} :ol.llx/fn]
    [:start-source! :ol.llx/fn]
    [:open-stream! :ol.llx/fn]]

   :ol.llx/runtime-run-stream-result
   [:map {:closed true}
    [:cancel-fn :ol.llx/fn]
    [:done? :ol.llx/fn]
    [:payload-ch {:optional true} :ol.llx/stream-channel]
    [:control-ch {:optional true} :ol.llx/stream-channel]]

   :ol.llx/runtime-run-stream-args
   :ol.llx/runtime-run-stream-base-input})
