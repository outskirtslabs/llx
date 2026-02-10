(ns llx-ai.client
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [llx-ai.adapters.anthropic-messages :as anthropic-messages]
   [llx-ai.adapters.google-generative-ai :as google-generative-ai]
   [llx-ai.adapters.openai-completions :as openai-completions]
   [llx-ai.adapters.openai-responses :as openai-responses]
   [llx-ai.errors :as errors]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.models :as models]
   [llx-ai.registry :as registry]
   [llx-ai.schema :as schema]
   [llx-ai.transform-messages :as transform-messages]
   [taoensso.trove :as trove]))

(def ^:private builtins-source-id "llx-ai.client/builtins")

(def default-registry
  (-> (registry/immutable-registry)
      (registry/register-adapter
       (openai-completions/adapter)
       builtins-source-id)
      (registry/register-adapter
       (openai-responses/adapter)
       builtins-source-id)
      (registry/register-adapter
       (anthropic-messages/adapter)
       builtins-source-id)
      (registry/register-adapter
       (google-generative-ai/adapter)
       builtins-source-id)))

(defn- assert-context!
  [context]
  (schema/assert-valid! :llx/context-map context))

(defn- split-client-opts
  [opts]
  {:registry-override (:registry opts)
   :request-opts      (dissoc opts :registry)})

(defn- resolve-call-registry
  [env registry-override]
  (or registry-override
      (:registry env)
      default-registry))

(defn- clamp-reasoning-level
  [model level]
  (if (and (= :xhigh level) (not (models/supports-xhigh? model)))
    :high
    level))

(defn- simple-opts->request-opts
  [model simple-opts]
  (let [simple-opts       (or simple-opts {})
        max-output-tokens (or (:max-tokens simple-opts)
                              (min (long (:max-tokens model)) 32000))
        reasoning-level   (or (:reasoning simple-opts) (:reasoning-effort simple-opts))
        reasoning-level   (when reasoning-level (clamp-reasoning-level model reasoning-level))]
    (cond-> {:max-output-tokens max-output-tokens}
      (contains? simple-opts :temperature) (assoc :temperature (:temperature simple-opts))
      (contains? simple-opts :top-p) (assoc :top-p (:top-p simple-opts))
      (contains? simple-opts :api-key) (assoc :api-key (:api-key simple-opts))
      (contains? simple-opts :headers) (assoc :headers (:headers simple-opts))
      (contains? simple-opts :signal) (assoc :signal (:signal simple-opts))
      (contains? simple-opts :metadata) (assoc :metadata (:metadata simple-opts))
      (contains? simple-opts :registry) (assoc :registry (:registry simple-opts))
      reasoning-level (assoc :reasoning {:level reasoning-level}))))

(defn- assert-reasoning-level!
  [model opts]
  (when (= :xhigh (get-in opts [:reasoning :level]))
    (when-not (models/supports-xhigh? model)
      (throw (errors/unsupported-reasoning-level (:id model) :xhigh)))))

(defn- select-adapter
  [resolved-registry model]
  (let [api     (:api model)
        adapter (registry/get-adapter resolved-registry api)]
    (when-not adapter
      (throw (ex-info "No adapter registered for api" {:api api})))
    (when-not (= api (:api adapter))
      (throw (ex-info "Mismatched api" {:model-api api :adapter-api (:api adapter)})))
    (when-let [supports-model? (:supports-model? adapter)]
      (when-not (supports-model? model)
        (throw (ex-info "Adapter does not support model" {:api api :model (:id model)}))))
    adapter))

(defn- apply-message-transform
  [env adapter model context]
  (let [transform-options (cond-> {:clock/now-ms (:clock/now-ms env)}
                            (:normalize-tool-call-id adapter)
                            (assoc :normalize-tool-call-id (:normalize-tool-call-id adapter))

                            (map? (:transform-options adapter))
                            (merge (:transform-options adapter)))
        transformed       (assoc context
                                 :messages
                                 (transform-messages/for-target-model
                                  (:messages context)
                                  model
                                  transform-options))]
    (if-let [transform-context-fn (:transform-context adapter)]
      (transform-context-fn model transformed)
      transformed)))

(defn- check-response-status!
  [env model response operation]
  (let [status (long (or (:status response) 0))]
    (when (or (< status 200) (>= status 300))
      (let [body-str      (cond
                            (string? (:body response)) (:body response)
                            (nil? (:body response)) ""
                            :else (if-let [f (:http/read-body-string env)]
                                    (f (:body response)) ""))
            body          ((:json/decode-safe env) body-str {})
            headers       (:headers response)
            provider      (name (or (:provider model) "unknown"))
            message       (or (get-in body [:error :message]) body-str)
            provider-code (get-in body [:error :type])
            request-id    (get headers "x-request-id")
            retry-after   (errors/extract-retry-after-hint headers message)]
        (trove/log! {:level :info
                     :id    :llx.obs/http-status-error
                     :data  {:call-id       (:call/id env)
                             :operation     operation
                             :provider      (:provider model)
                             :api           (:api model)
                             :model-id      (:id model)
                             :status        status
                             :request-id    request-id
                             :provider-code provider-code
                             :retry-after   retry-after
                             :error-type    (:type (ex-data (errors/http-status->error
                                                             status provider message
                                                             :provider-code provider-code
                                                             :retry-after retry-after
                                                             :request-id request-id
                                                             :body body)))}})
        (throw (errors/http-status->error
                status provider message
                :provider-code provider-code
                :retry-after retry-after
                :request-id request-id
                :body body))))))

(>defn complete
       "Runs a non-streaming completion request and returns the canonical assistant message."
       [env model context opts]
       [:llx/env :llx/model :llx/context-map [:maybe :llx/request-options] => :llx/message-assistant]
       (let [_                         (schema/assert-valid! [:maybe :llx/request-options] opts)
             opts                      (or opts {})
             {:keys [registry-override
                     request-opts]}    (split-client-opts opts)
             _                         (schema/assert-valid! :llx/env env)
             _                         (schema/assert-valid! :llx/model model)
             _                         (schema/assert-valid! :llx/request-options request-opts)
             _                         (assert-reasoning-level! model request-opts)
             context                   (assert-context! context)
             call-env                  (assoc env :call/id ((:id/new env)))
             resolved-registry         (resolve-call-registry env registry-override)
             adapter                   (select-adapter resolved-registry model)
             context                   (apply-message-transform env adapter model context)
             request                   (schema/assert-valid!
                                        :llx/adapter-request-map
                                        ((:build-request adapter) call-env model context request-opts false))
             max-retries               (get request-opts :max-retries 2)
             sleep-fn                  (or (:thread/sleep env)
                                           (when (pos? max-retries)
                                             (throw (ex-info "Retry requested but env is missing :thread/sleep"
                                                             {:type        :llx/config-error
                                                              :max-retries max-retries}))))
             do-request                (fn []
                                         (let [response ((:http/request env) request)]
                                           (check-response-status! call-env model response :complete)
                                           response))]
         (trove/log! {:level :info
                      :id    :llx.obs/call-start
                      :data  {:call-id            (:call/id call-env)
                              :operation          :complete
                              :provider           (:provider model)
                              :api                (:api model)
                              :model-id           (:id model)
                              :message-count      (count (:messages context))
                              :has-tools?         (boolean (seq (:tools context)))
                              :has-system-prompt? (boolean (seq (:system-prompt context)))}})
         (try
           (let [response                    (errors/retry-loop do-request max-retries sleep-fn
                                                                {:call-id  (:call/id call-env)
                                                                 :provider (:provider model)})
                 {:keys [assistant-message]} (schema/assert-valid!
                                              :llx/runtime-finalize-result
                                              ((:finalize adapter) call-env {:model model :response response}))]
             (trove/log! {:level :info
                          :id    :llx.obs/call-finished
                          :data  {:call-id             (:call/id call-env)
                                  :operation           :complete
                                  :provider            (:provider model)
                                  :api                 (:api model)
                                  :model-id            (:id model)
                                  :stop-reason         (:stop-reason assistant-message)
                                  :usage               (:usage assistant-message)
                                  :content-block-count (count (:content assistant-message))}})
             assistant-message)
           (catch #?(:clj Exception :cljs :default) ex
             (trove/log! {:level :error
                          :id    :llx.obs/call-error
                          :data  {:call-id       (:call/id call-env)
                                  :operation     :complete
                                  :provider      (:provider model)
                                  :api           (:api model)
                                  :model-id      (:id model)
                                  :error-type    (:type (ex-data ex))
                                  :error-message (ex-message ex)
                                  :recoverable?  (get (ex-data ex) :recoverable?)
                                  :request-id    (get (ex-data ex) :request-id)
                                  :provider-code (get (ex-data ex) :provider-code)}})
             (throw ex)))))

(>defn stream
       "Runs a streaming completion request and returns an LLX event-stream map."
       [env model context opts]
       [:llx/env :llx/model :llx/context-map [:maybe :llx/request-options] => :llx/event-stream-map]
       (let [_                         (schema/assert-valid! [:maybe :llx/request-options] opts)
             opts                      (or opts {})
             {:keys [registry-override
                     request-opts]}    (split-client-opts opts)
             _                         (schema/assert-valid! :llx/env env)
             _                         (schema/assert-valid! :llx/model model)
             _                         (schema/assert-valid! :llx/request-options request-opts)
             _                         (assert-reasoning-level! model request-opts)
             context                   (assert-context! context)
             call-env                  (assoc env :call/id ((:id/new env)))
             resolved-registry         (resolve-call-registry env registry-override)
             adapter                   (select-adapter resolved-registry model)
             context                   (apply-message-transform env adapter model context)
             request                   (schema/assert-valid!
                                        :llx/adapter-request-map
                                        ((:build-request adapter) call-env model context request-opts true))
             out                       (event-stream/assistant-message-stream)
             state*                    (atom {:model model})
             run-stream!               (:stream/run! env)]
         (when-not run-stream!
           (throw (ex-info "Environment missing :stream/run! runtime hook" {})))
         (trove/log! {:level :info
                      :id    :llx.obs/call-start
                      :data  {:call-id            (:call/id call-env)
                              :operation          :stream
                              :provider           (:provider model)
                              :api                (:api model)
                              :model-id           (:id model)
                              :message-count      (count (:messages context))
                              :has-tools?         (boolean (seq (:tools context)))
                              :has-system-prompt? (boolean (seq (:system-prompt context)))}})
         (try
           (run-stream! {:adapter      adapter
                         :env          call-env
                         :model        model
                         :request      request
                         :out          out
                         :state*       state*
                         :request-opts request-opts})
           out
           (catch #?(:clj Exception :cljs :default) ex
             (trove/log! {:level :error
                          :id    :llx.obs/call-error
                          :data  {:call-id       (:call/id call-env)
                                  :operation     :stream
                                  :provider      (:provider model)
                                  :api           (:api model)
                                  :model-id      (:id model)
                                  :error-type    (:type (ex-data ex))
                                  :error-message (ex-message ex)
                                  :recoverable?  (get (ex-data ex) :recoverable?)
                                  :request-id    (get (ex-data ex) :request-id)
                                  :provider-code (get (ex-data ex) :provider-code)}})
             (throw ex)))))

(defn stream-simple
  "Runs [[stream]] with normalized simple options.

  Options:

  | key | description |
  | --- | --- |
  | `:temperature` | Sampling temperature. |
  | `:top-p` | Nucleus sampling probability. |
  | `:max-tokens` | Requested output cap; maps to `:max-output-tokens`. |
  | `:reasoning` | Reasoning level keyword; `:xhigh` clamped to `:high` unless model supports it. |
  | `:reasoning-effort` | Alias for `:reasoning`. |
  | `:api-key` | Provider API key override. |
  | `:headers` | Additional request headers. |
  | `:signal` | Abort signal propagated to runtime. |
  | `:metadata` | Request metadata map. |
  | `:registry` | Adapter registry override for this call. |"
  [env model context simple-opts]
  (stream env model context (simple-opts->request-opts model simple-opts)))

(defn complete-simple
  "Runs [[complete]] with normalized simple options.

  Accepts the same option keys as [[stream-simple]]."
  [env model context simple-opts]
  (complete env model context (simple-opts->request-opts model simple-opts)))
