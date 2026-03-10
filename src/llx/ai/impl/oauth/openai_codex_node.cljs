(ns llx.ai.impl.oauth.openai-codex-node)

(defn- unsupported!
  [operation]
  (throw (ex-info "OpenAI Codex OAuth is not implemented for this host"
                  {:provider  "openai-codex"
                   :operation operation})))

(defn login-openai-codex
  [_callbacks]
  (unsupported! :login))

(defn refresh-openai-codex-token
  [_refresh-token]
  (unsupported! :refresh-token))

(def oauth-provider
  {:id                    "openai-codex"
   :name                  "ChatGPT Plus/Pro (Codex Subscription)"
   :uses-callback-server? true
   :login                 login-openai-codex
   :refresh-token         (fn [credentials]
                            (refresh-openai-codex-token (:refresh credentials)))
   :get-api-key           (fn [credentials]
                            (:access credentials))})
