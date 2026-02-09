(ns llx-ai.utils.tool-validation
  (:require
   [malli.core :as m]
   [malli.error :as me]))

(defn- find-tool
  [tools tool-name]
  (some #(when (= (:name %) tool-name) %) tools))

(defn validate-tool-call
  "Validates tool-call arguments against the matching tool `:input-schema`.

  Returns validated arguments on success.

  Throws `ExceptionInfo` with structured `ex-data`:
  - `{:type :tool-not-found ...}` when `tool-call` name is unknown.
  - `{:type :validation-error ...}` when arguments do not satisfy the schema."
  [tools tool-call]
  (let [tool-name (:name tool-call)
        tool      (find-tool tools tool-name)]
    (when-not tool
      (throw
       (ex-info "Tool not found"
                {:type            :tool-not-found
                 :tool-name       tool-name
                 :available-tools (->> tools (mapv :name) sort)})))
    (let [args   (:arguments tool-call)
          schema (:input-schema tool)]
      (if (m/validate schema args)
        args
        (throw
         (ex-info "Tool call arguments failed validation"
                  {:type      :validation-error
                   :tool-name tool-name
                   :arguments args
                   :errors    (me/humanize (m/explain schema args))}))))))
