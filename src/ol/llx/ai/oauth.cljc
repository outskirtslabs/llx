(ns ol.llx.ai.oauth
  (:require
   [ol.llx.ai.impl.oauth.core :as oauth-core]
   [ol.llx.ai.impl.oauth.registry :as oauth-registry]
   #?(:clj [ol.llx.ai.impl.oauth.openai-codex-jvm :as openai-codex]
      :cljs [ol.llx.ai.impl.oauth.openai-codex-node :as openai-codex])))

(defonce ^:private _builtins-initialized
  (do
    (oauth-registry/register-oauth-provider! openai-codex/oauth-provider)
    true))

(defn register-oauth-provider!
  [provider]
  (oauth-registry/register-oauth-provider! provider))

(defn get-oauth-provider
  [provider-id]
  (oauth-registry/get-oauth-provider provider-id))

(defn get-oauth-providers
  []
  (oauth-registry/get-oauth-providers))

(defn refresh-oauth-token
  [provider-id credentials]
  (oauth-core/refresh-oauth-token provider-id credentials))

(defn get-oauth-api-key
  ([provider-id credentials-by-provider]
   (oauth-core/get-oauth-api-key provider-id credentials-by-provider))
  ([provider-id credentials-by-provider opts]
   (oauth-core/get-oauth-api-key provider-id credentials-by-provider opts)))
