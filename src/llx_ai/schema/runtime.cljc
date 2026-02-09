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
    [:transform-context {:optional true} :llx/fn]]

   :llx/env
   [:map {:closed true}
    [:http/request :llx/fn]
    [:json/encode :llx/fn]
    [:json/decode :llx/fn]
    [:clock/now-ms :llx/fn]
    [:id/new :llx/fn]
    [:env/get {:optional true} :llx/fn]
    [:log/debug {:optional true} :llx/fn]
    [:log/warn {:optional true} :llx/fn]
    [:log/error {:optional true} :llx/fn]]})
