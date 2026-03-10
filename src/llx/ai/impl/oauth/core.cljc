(ns llx.ai.impl.oauth.core
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [llx.ai.impl.oauth.registry :as registry]
   [llx.ai.impl.schema :as schema]))

(defn- now-ms
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

(defn- expired?
  [credentials now-ms-fn]
  (>= (long (or (now-ms-fn) 0))
      (long (or (:expires credentials) 0))))

(>defn refresh-oauth-token
       [provider-id credentials]
       [:llx/oauth-provider-id :llx/oauth-credentials => :llx/oauth-credentials]
       (let [provider (registry/get-oauth-provider provider-id)]
         (when-not provider
           (throw (ex-info "Unknown OAuth provider" {:provider-id provider-id})))
         (let [updated ((:refresh-token provider) credentials)]
           (schema/assert-valid! :llx/oauth-credentials updated))))

(>defn get-oauth-api-key
       ([provider-id credentials-by-provider]
        [:llx/oauth-provider-id :llx/oauth-credentials-by-provider
         => [:maybe
             [:map
              [:new-credentials :llx/oauth-credentials]
              [:api-key :llx/id-string]]]]
        (get-oauth-api-key provider-id credentials-by-provider {}))
       ([provider-id credentials-by-provider opts]
        [:llx/oauth-provider-id
         :llx/oauth-credentials-by-provider
         [:map {:closed false}
          [:now-ms {:optional true} :llx/fn]]
         => [:maybe
             [:map
              [:new-credentials :llx/oauth-credentials]
              [:api-key :llx/id-string]]]]
        (let [provider (registry/get-oauth-provider provider-id)]
          (when-not provider
            (throw (ex-info "Unknown OAuth provider" {:provider-id provider-id})))
          (when-let [credentials (get credentials-by-provider provider-id)]
            (let [now-ms-fn   (or (:now-ms opts) now-ms)
                  credentials (if (expired? credentials now-ms-fn)
                                (refresh-oauth-token provider-id credentials)
                                credentials)
                  api-key     ((:get-api-key provider) credentials)]
              (when-not (seq (or api-key ""))
                (throw (ex-info "OAuth provider returned empty API key"
                                {:provider-id provider-id})))
              {:new-credentials credentials
               :api-key         api-key})))))
