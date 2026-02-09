(ns llx-ai.client
  (:require
   [llx-ai.adapters.openai-completions :as openai-completions]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.registry :as registry]
   [llx-ai.schema :as schema]))

(def ^:private builtins-source-id "llx-ai.client/builtins")

(defn register-builtins!
  []
  (registry/register! (openai-completions/adapter) builtins-source-id))

(register-builtins!)

(defn- assert-context!
  [context]
  (let [messages (:messages context)]
    (when-not (vector? messages)
      (throw (ex-info "Context must contain :messages vector" {:context context})))
    (doseq [message messages]
      (schema/assert-valid! :llx/message message))
    context))

(defn- resolve-adapter!
  [model]
  (let [api     (:api model)
        adapter (registry/get-adapter api)]
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
        _                           (schema/assert-valid! :llx/env env)
        _                           (schema/assert-valid! :llx/model model)
        _                           (schema/assert-valid! :llx/request-options opts)
        context                     (assert-context! context)
        adapter                     (resolve-adapter! model)
        context                     (transform-context adapter model context)
        request                     ((:build-request adapter) env model context opts false)
        response                    ((:http/request env) request)
        {:keys [assistant-message]} ((:finalize adapter) env {:model model :response response})]
    assistant-message))

(defn stream
  [env model context opts]
  (let [opts        (or opts {})
        _           (schema/assert-valid! :llx/env env)
        _           (schema/assert-valid! :llx/model model)
        _           (schema/assert-valid! :llx/request-options opts)
        context     (assert-context! context)
        adapter     (resolve-adapter! model)
        context     (transform-context adapter model context)
        request     ((:build-request adapter) env model context opts true)
        out         (event-stream/assistant-message-stream)
        state*      (atom {:model model})
        run-stream! (:stream/run! env)]
    (when-not run-stream!
      (throw (ex-info "Environment missing :stream/run! runtime hook" {})))
    (run-stream! {:adapter adapter
                  :env     env
                  :model   model
                  :request request
                  :out     out
                  :state*  state*})
    out))
