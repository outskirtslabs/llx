(ns llx-ai.client
  (:require
   [llx-ai.adapters.openai-completions :as openai-completions]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.registry :as registry]
   [llx-ai.schema :as schema]))

(def ^:private builtins-source-id "llx-ai.client/builtins")

(def default-registry
  (registry/register-adapter
   (registry/immutable-registry)
   (openai-completions/adapter)
   builtins-source-id))

(defn- assert-context!
  [context]
  (let [messages (:messages context)]
    (when-not (vector? messages)
      (throw (ex-info "Context must contain :messages vector" {:context context})))
    (doseq [message messages]
      (schema/assert-valid! :llx/message message))
    context))

(defn- split-client-opts
  [opts]
  {:registry-override (:registry opts)
   :request-opts      (dissoc opts :registry)})

(defn- resolve-call-registry
  [env registry-override]
  (or registry-override
      (:registry env)
      default-registry))

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

(defn- transform-context
  [adapter model context]
  (if-let [transform-context-fn (:transform-context adapter)]
    (transform-context-fn model context)
    context))

(defn complete
  [env model context opts]
  (let [opts                        (or opts {})
        {:keys [registry-override
                request-opts]}      (split-client-opts opts)
        _                           (schema/assert-valid! :llx/env env)
        _                           (schema/assert-valid! :llx/model model)
        _                           (schema/assert-valid! :llx/request-options request-opts)
        context                     (assert-context! context)
        resolved-registry           (resolve-call-registry env registry-override)
        adapter                     (select-adapter resolved-registry model)
        context                     (transform-context adapter model context)
        request                     ((:build-request adapter) env model context request-opts false)
        response                    ((:http/request env) request)
        {:keys [assistant-message]} ((:finalize adapter) env {:model model :response response})]
    assistant-message))

(defn stream
  [env model context opts]
  (let [opts                      (or opts {})
        {:keys [registry-override
                request-opts]}    (split-client-opts opts)
        _                         (schema/assert-valid! :llx/env env)
        _                         (schema/assert-valid! :llx/model model)
        _                         (schema/assert-valid! :llx/request-options request-opts)
        context                   (assert-context! context)
        resolved-registry         (resolve-call-registry env registry-override)
        adapter                   (select-adapter resolved-registry model)
        context                   (transform-context adapter model context)
        request                   ((:build-request adapter) env model context request-opts true)
        out                       (event-stream/assistant-message-stream)
        state*                    (atom {:model model})
        run-stream!               (:stream/run! env)]
    (when-not run-stream!
      (throw (ex-info "Environment missing :stream/run! runtime hook" {})))
    (run-stream! {:adapter adapter
                  :env     env
                  :model   model
                  :request request
                  :out     out
                  :state*  state*})
    out))
