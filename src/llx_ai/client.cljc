(ns llx-ai.client
  (:require
   [llx-ai.adapters.openai-completions :as openai-completions]
   [llx-ai.schema :as schema]))

(defn- assert-context!
  [context]
  (let [messages (:messages context)]
    (when-not (vector? messages)
      (throw (ex-info "Context must contain :messages vector" {:context context})))
    (doseq [message messages]
      (schema/assert-valid! :llx/message message))
    context))

(defn complete
  [env model context opts]
  (let [opts (or opts {})]
    (schema/assert-valid! :llx/env env)
    (schema/assert-valid! :llx/model model)
    (schema/assert-valid! :llx/request-options opts)
    (assert-context! context)
    (case (:api model)
      :openai-completions (openai-completions/complete* env model context opts)
      (throw (ex-info "Unsupported api" {:api (:api model)})))))
