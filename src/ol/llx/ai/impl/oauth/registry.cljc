(ns ol.llx.ai.impl.oauth.registry
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [ol.llx.ai.impl.schema :as schema]))

(defonce ^:private providers* (atom {}))

(>defn register-oauth-provider!
       [provider]
       [:ol.llx/oauth-provider => :ol.llx/oauth-provider]
       (let [provider (schema/assert-valid! :ol.llx/oauth-provider provider)
             id       (:id provider)]
         (swap! providers* assoc id provider)
         provider))

(>defn get-oauth-provider
       [provider-id]
       [:ol.llx/oauth-provider-id => [:maybe :ol.llx/oauth-provider]]
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
