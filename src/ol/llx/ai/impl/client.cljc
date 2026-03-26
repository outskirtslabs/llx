(ns ol.llx.ai.impl.client
  (:require
   [clojure.string :as str]
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [ol.llx.ai.impl.adapters.anthropic-messages :as anthropic-messages]
   [ol.llx.ai.impl.adapters.google-generative-ai :as google-generative-ai]
   [ol.llx.ai.impl.adapters.openai-codex-responses :as openai-codex-responses]
   [ol.llx.ai.impl.adapters.openai-completions :as openai-completions]
   [ol.llx.ai.impl.adapters.openai-responses :as openai-responses]
   [ol.llx.ai.impl.errors :as errors]
   [ol.llx.ai.impl.models :as models]
   [ol.llx.ai.impl.registry :as registry]
   [ol.llx.ai.impl.schema :as schema]
   [ol.llx.ai.impl.transform-messages :as transform-messages]
   [malli.core :as m]
   [malli.transform :as mt]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]
   [taoensso.trove :as trove]))

(def ^:private builtins-source-id "ol.llx.ai.impl.client/builtins")

(def default-registry
  (-> (registry/immutable-registry)
      (registry/register-adapter
       (openai-completions/adapter)
       builtins-source-id)
      (registry/register-adapter
       (openai-responses/adapter)
       builtins-source-id)
      (registry/register-adapter
       (openai-codex-responses/adapter)
       builtins-source-id)
      (registry/register-adapter
       (anthropic-messages/adapter)
       builtins-source-id)
      (registry/register-adapter
       (google-generative-ai/adapter)
       builtins-source-id)))

(def ^:private default-transformer
  (mt/default-value-transformer {::mt/add-optional-keys true}))

(def ^:private unified->provider-passthrough-keys
  [:session-id
   :temperature
   :top-p
   :thinking-budgets
   :api-key
   :headers
   :signal
   :max-retry-delay-ms
   :metadata
   :registry])

(defn- assert-context!
  [context]
  (schema/assert-valid! :ol.llx/context-map context))

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

(defn- reasoning-option
  [model unified-opts]
  (let [reasoning-level (or (:reasoning unified-opts)
                            (:reasoning-effort unified-opts))
        reasoning-level (when reasoning-level
                          (clamp-reasoning-level model reasoning-level))]
    (when reasoning-level
      (if (#{:openai-responses :openai-codex-responses} (:api model))
        {:effort reasoning-level}
        {:level reasoning-level}))))

(>defn unified-opts->request-opts
       "Converts unified options to provider-path request options.

  See [[ol.llx.ai/complete]] and [[ol.llx.ai/stream]] for unified option semantics."
       [model unified-opts]
       [:ol.llx/model [:maybe :ol.llx/unified-request-options] => :ol.llx/provider-request-options]
       (let [registry          (schema/registry)
             unified-opts      (m/coerce :ol.llx/unified-request-options (or unified-opts {}) default-transformer {:registry registry})
             max-output-tokens (or (:max-tokens unified-opts)
                                   (min (long (:max-tokens model)) 32000))
             reasoning         (reasoning-option model unified-opts)
             request-opts      (merge
                                {:max-output-tokens max-output-tokens}
                                (select-keys unified-opts unified->provider-passthrough-keys)
                                (when reasoning {:reasoning reasoning}))]
         (m/coerce :ol.llx/provider-request-options
                   request-opts
                   default-transformer
                   {:registry registry})))

(defn- assert-reasoning-level!
  [model opts]
  (when (= :xhigh (or (get-in opts [:reasoning :level])
                      (get-in opts [:reasoning :effort])))
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

(defn provider-from-host
  [host]
  (let [host (str/lower-case (str (or host "")))]
    (cond
      (str/blank? host) "unknown"
      (str/includes? host "chatgpt.com") "openai-codex"
      (str/includes? host "openai.com") "openai"
      (str/includes? host "anthropic.com") "anthropic"
      (str/includes? host "googleapis.com") "google"
      (str/includes? host "mistral.ai") "mistral"
      :else host)))

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
        (trove/log! {:level :debug
                     :id    :ol.llx.obs/http-status-error
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

(defn- start-channel-close-watcher!
  [out runtime-cancel* runtime-done?*]
  (letfn [(poll []
            (if (sp/closed? out)
              (when (and (fn? @runtime-cancel*)
                         (not ((or @runtime-done?* (constantly false)))))
                (@runtime-cancel*))
              (-> (p/delay 10 nil)
                  (p/then (fn [_]
                            (poll))))))]
    (poll)))

(defn runtime-data-line->payload
  [line]
  (when (str/starts-with? line "data:")
    (let [payload (str/trim (subs line (count "data:")))]
      (when (and (seq payload) (not= payload "[DONE]"))
        payload))))

(defn runtime-fallback-error-message
  [stream-ex normalize-ex]
  (str "Stream error: " (or (ex-message stream-ex) (pr-str stream-ex))
       (when normalize-ex
         (str " | normalize-error failure: " (or (ex-message normalize-ex)
                                                 (pr-str normalize-ex))))))

(defn runtime-fallback-assistant-message
  [env model stream-ex normalize-ex]
  {:role          :assistant
   :content       []
   :api           (:api model)
   :provider      (:provider model)
   :model         (:id model)
   :usage         {:input        0
                   :output       0
                   :cache-read   0
                   :cache-write  0
                   :total-tokens 0
                   :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
   :stop-reason   :error
   :error-message (runtime-fallback-error-message stream-ex normalize-ex)
   :timestamp     ((:clock/now-ms env))})

(defn runtime-payload->provider-item-type
  [env payload]
  (let [decoded (or (when-let [decode-safe (:json/decode-safe env)]
                      (decode-safe payload {:key-fn keyword}))
                    {})]
    (or (:type decoded)
        (when (contains? decoded :candidates) "candidates")
        (when (contains? decoded :choices) "choices"))))

(defn runtime-split-lines
  [buffer]
  (let [parts (str/split buffer #"\n" -1)]
    {:lines     (butlast parts)
     :remainder (last parts)}))

(defn- ensure-streaming-error
  [model ex]
  (if (errors/llx-error? ex)
    ex
    (let [provider      (name (or (:provider model) "unknown"))
          error-message (or (ex-message ex) (pr-str ex))
          wrapped       (errors/streaming-error provider error-message)
          wrapped-data  (ex-data wrapped)
          original-data (if (map? (ex-data ex)) (ex-data ex) {})]
      (ex-info (ex-message wrapped)
               (merge wrapped-data
                      (dissoc original-data :type :message :recoverable? :provider))))))

(defn runtime-terminal-error-event
  [adapter env model state* stream-ex]
  (let [stream-ex                             (ensure-streaming-error model stream-ex)
        [assistant-message normalize-failed?] (try
                                                [(schema/assert-valid!
                                                  :ol.llx/message-assistant
                                                  ((:normalize-error adapter) env stream-ex @state*))
                                                 false]
                                                (catch #?(:clj Exception :cljs :default) normalize-ex
                                                  [(runtime-fallback-assistant-message
                                                    env model stream-ex normalize-ex)
                                                   true]))]
    (trove/log! {:level :error
                 :id    :ol.llx.obs/stream-event-error
                 :data  {:call-id                 (:call/id env)
                         :provider                (:provider model)
                         :api                     (:api model)
                         :model-id                (:id model)
                         :error-type              (:type (ex-data stream-ex))
                         :error-message           (ex-message stream-ex)
                         :normalize-error-failed? normalize-failed?}})
    {:type :error :assistant-message assistant-message}))

(>defn complete*
       "See [[ol.llx.ai/complete*]]"
       [env model context opts]
       [:ol.llx/env :ol.llx/model :ol.llx/context-map [:maybe :ol.llx/provider-request-options] => :ol.llx/deferred]
       (try
         (let [_                         (schema/assert-valid! [:maybe :ol.llx/provider-request-options] opts)
               opts                      (or opts {})
               {:keys [registry-override
                       request-opts]}    (split-client-opts opts)
               _                         (schema/assert-valid! :ol.llx/env env)
               _                         (schema/assert-valid! :ol.llx/model model)
               _                         (schema/assert-valid! :ol.llx/provider-request-options request-opts)
               _                         (assert-reasoning-level! model request-opts)
               context                   (assert-context! context)
               call-env                  (assoc env :call/id ((:id/new env)))
               resolved-registry         (resolve-call-registry env registry-override)
               adapter                   (select-adapter resolved-registry model)
               context                   (apply-message-transform env adapter model context)
               request                   (schema/assert-valid!
                                          :ol.llx/adapter-request-map
                                          ((:build-request adapter) call-env model context request-opts false))
               max-retries               (get request-opts :max-retries 2)
               do-request                (fn []
                                           (let [response ((:http/request env) request)
                                                 handle   (fn [response]
                                                            (check-response-status! call-env model response :complete)
                                                            response)]
                                             (if (p/deferred? response)
                                               (p/then response handle)
                                               (p/resolved (handle response)))))]
           (trove/log! {:level :debug
                        :id    :ol.llx.obs/call-start
                        :data  {:call-id            (:call/id call-env)
                                :operation          :complete
                                :provider           (:provider model)
                                :api                (:api model)
                                :model-id           (:id model)
                                :message-count      (count (:messages context))
                                :has-tools?         (boolean (seq (:tools context)))
                                :has-system-prompt? (boolean (seq (:system-prompt context)))}})
           (-> (errors/retry-loop-async do-request max-retries p/delay
                                        {:call-id            (:call/id call-env)
                                         :provider           (:provider model)
                                         :max-retry-delay-ms (:max-retry-delay-ms request-opts)})
               (p/then (fn [response]
                         (let [{:keys [assistant-message]} (schema/assert-valid!
                                                            :ol.llx/runtime-finalize-result
                                                            ((:finalize adapter) call-env {:model model :response response}))]
                           (trove/log! {:level :debug
                                        :id    :ol.llx.obs/call-finished
                                        :data  {:call-id             (:call/id call-env)
                                                :operation           :complete
                                                :provider            (:provider model)
                                                :api                 (:api model)
                                                :model-id            (:id model)
                                                :stop-reason         (:stop-reason assistant-message)
                                                :usage               (:usage assistant-message)
                                                :content-block-count (count (:content assistant-message))}})
                           assistant-message)))
               (p/catch (fn [ex]
                          (trove/log! {:level :error
                                       :id    :ol.llx.obs/call-error
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
                          (p/rejected ex)))))
         (catch #?(:clj Exception :cljs :default) ex
           (p/rejected ex))))

(>defn stream*
       "See [[ol.llx.ai/stream*]]"
       [env model context opts]
       [:ol.llx/env :ol.llx/model :ol.llx/context-map [:maybe :ol.llx/provider-request-options] => :ol.llx/stream-channel]
       (let [_                         (schema/assert-valid! [:maybe :ol.llx/provider-request-options] opts)
             opts                      (or opts {})
             {:keys [registry-override
                     request-opts]}    (split-client-opts opts)
             _                         (schema/assert-valid! :ol.llx/env env)
             _                         (schema/assert-valid! :ol.llx/model model)
             _                         (schema/assert-valid! :ol.llx/provider-request-options request-opts)
             _                         (assert-reasoning-level! model request-opts)
             context                   (assert-context! context)
             call-env                  (assoc env :call/id ((:id/new env)))
             resolved-registry         (resolve-call-registry env registry-override)
             adapter                   (select-adapter resolved-registry model)
             context                   (apply-message-transform env adapter model context)
             request                   (schema/assert-valid!
                                        :ol.llx/adapter-request-map
                                        ((:build-request adapter) call-env model context request-opts true))
             state*                    (atom {:model model})
             run-stream!               (:stream/run! env)
             runtime-cancel*           (atom nil)
             runtime-done?*            (atom (constantly false))
             out                       (sp/chan :buf (sp/sliding-buffer 64))]
         (when-not run-stream!
           (throw (ex-info "Environment missing :stream/run! runtime hook" {})))
         (trove/log! {:level :debug
                      :id    :ol.llx.obs/call-start
                      :data  {:call-id            (:call/id call-env)
                              :operation          :stream
                              :provider           (:provider model)
                              :api                (:api model)
                              :model-id           (:id model)
                              :message-count      (count (:messages context))
                              :has-tools?         (boolean (seq (:tools context)))
                              :has-system-prompt? (boolean (seq (:system-prompt context)))}})
         (try
           (let [runtime-result (run-stream! {:adapter      adapter
                                              :env          call-env
                                              :model        model
                                              :request      request
                                              :out          out
                                              :state*       state*
                                              :request-opts request-opts})]
             (cond
               (fn? runtime-result)
               (reset! runtime-cancel* runtime-result)

               (map? runtime-result)
               (do
                 (reset! runtime-cancel* (:cancel-fn runtime-result))
                 (reset! runtime-done?* (or (:done? runtime-result) (constantly false))))

               :else nil))
           (start-channel-close-watcher! out runtime-cancel* runtime-done?*)
           out
           (catch #?(:clj Exception :cljs :default) ex
             (trove/log! {:level :error
                          :id    :ol.llx.obs/call-error
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

(>defn stream
       "See [[ol.llx.ai/stream]]"
       [env model context unified-opts]
       [:ol.llx/env :ol.llx/model :ol.llx/context-map [:maybe :ol.llx/unified-request-options] => :ol.llx/stream-channel]
       (let [_ (schema/assert-valid! [:maybe :ol.llx/unified-request-options] unified-opts)]
         (stream* env model context (unified-opts->request-opts model unified-opts))))

(>defn complete
       "See [[ol.llx.ai/complete]]"
       [env model context unified-opts]
       [:ol.llx/env :ol.llx/model :ol.llx/context-map [:maybe :ol.llx/unified-request-options] => :ol.llx/deferred]
       (let [_ (schema/assert-valid! [:maybe :ol.llx/unified-request-options] unified-opts)]
         (complete* env model context (unified-opts->request-opts model unified-opts))))
