(ns llx.ai.impl.oauth.registry
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [llx.ai.impl.schema :as schema]))

(defonce ^:private providers* (atom {}))

(>defn register-oauth-provider!
       [provider]
       [:llx/oauth-provider => :llx/oauth-provider]
       (let [provider (schema/assert-valid! :llx/oauth-provider provider)
             id       (:id provider)]
         (swap! providers* assoc id provider)
         provider))

(>defn get-oauth-provider
       [provider-id]
       [:llx/oauth-provider-id => [:maybe :llx/oauth-provider]]
       (get @providers* provider-id))

(defn get-oauth-providers
  []
  (->> (vals @providers*)
       (sort-by :id)
       vec))

(defn clear-oauth-providers!
  []
  (reset! providers* {})
  nil)
