(ns llx.ai.impl.adapters.openai-codex-responses
  (:require
   [clojure.string :as str]
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [llx.ai.impl.adapters.openai-responses :as openai-responses]
   [llx.ai.impl.oauth.openai-codex :as openai-codex]))

(def ^:private default-base-url
  "https://chatgpt.com/backend-api")

(def ^:private default-originator
  "pi")

(def ^:private openai-beta-header
  "responses=experimental")

(defn- trim-trailing-slash
  [s]
  (str/replace (str (or s "")) #"/+$" ""))

(defn- resolve-codex-base-url
  [base-url]
  (let [normalized (trim-trailing-slash
                    (if (seq (str base-url))
                      base-url
                      default-base-url))]
    (cond
      (str/ends-with? normalized "/codex/responses")
      (subs normalized 0 (- (count normalized) (count "/responses")))

      (str/ends-with? normalized "/codex")
      normalized

      :else
      (str normalized "/codex"))))

(defn- bearer-token
  [authorization]
  (let [value (str (or authorization ""))]
    (when (str/starts-with? value "Bearer ")
      (subs value (count "Bearer ")))))

(defn- decode-body
  [env body]
  ((:json/decode env) body {:key-fn keyword}))

(defn- encode-body
  [env payload]
  ((:json/encode env) ((:unicode/sanitize-payload env) payload)))

(defn- account-id-from-request-token
  [request]
  (let [token      (bearer-token (get-in request [:headers "Authorization"]))
        account-id (openai-codex/account-id-from-access-token token)]
    (when-not (seq account-id)
      (throw (ex-info "Failed to extract account id from token" {:token-present? (boolean (seq token))})))
    account-id))

(>defn build-request
       ([env model context opts]
        [:llx/env :llx/model :llx/context-map :llx/openai-codex-responses-provider-options => :llx/adapter-request-map]
        (build-request env model context opts false))
       ([env model context opts stream?]
        [:llx/env :llx/model :llx/context-map :llx/openai-codex-responses-provider-options :boolean => :llx/adapter-request-map]
        (let [codex-model   (assoc model :base-url (resolve-codex-base-url (:base-url model)))
              system-prompt (:system-prompt context)
              request       (openai-responses/build-request env codex-model (dissoc context :system-prompt) opts stream?)
              account-id    (account-id-from-request-token request)
              session-id    (:session-id opts)
              payload       (decode-body env (:body request))
              payload       (-> payload
                                (dissoc :max_output_tokens :prompt_cache_retention)
                                (cond->
                                 (seq system-prompt)
                                  (assoc :instructions system-prompt)))
              headers       (cond-> (or (:headers request) {})
                              true (assoc "chatgpt-account-id" account-id
                                          "OpenAI-Beta" openai-beta-header
                                          "originator" (or (:originator opts) default-originator))

                              (seq session-id) (assoc "session_id" session-id
                                                      "conversation_id" session-id))]
          (assoc request
                 :headers headers
                 :body (encode-body env payload)))))

(def decode-event
  openai-responses/decode-event)

(def finalize
  openai-responses/finalize)

(def normalize-error
  openai-responses/normalize-error)

(def open-stream
  openai-responses/open-stream)

(def normalize-tool-call-id
  openai-responses/normalize-tool-call-id)

(defn adapter
  []
  {:api                    :openai-codex-responses
   :build-request          build-request
   :open-stream            open-stream
   :decode-event           decode-event
   :finalize               finalize
   :normalize-error        normalize-error
   :supports-model?        (fn [model] (= :openai-codex-responses (:api model)))
   :normalize-tool-call-id normalize-tool-call-id
   :transform-options      {:id-normalization-profile :openai-responses}
   :transform-context      (fn [_model context] context)})
