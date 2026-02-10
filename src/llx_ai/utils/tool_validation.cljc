(ns llx-ai.utils.tool-validation
  (:require
   [llx-ai.errors :as errors]
   [malli.core :as m]
   [malli.error :as me]))

(defn- find-tool
  [tools tool-name]
  (some #(when (= (:name %) tool-name) %) tools))

(defn validate-tool-call
  "Validates tool-call arguments against the matching tool `:input-schema`.

  Returns validated arguments on success.

  Throws `ExceptionInfo` with structured `ex-data`:
  - `{:type :llx/tool-not-found ...}` when `tool-call` name is unknown.
  - `{:type :llx/validation-error ...}` when arguments do not satisfy the schema."
  [tools tool-call]
  (let [tool-name (:name tool-call)
        tool      (find-tool tools tool-name)]
    (when-not tool
      (throw (errors/tool-not-found tool-name (->> tools (mapv :name)))))
    (let [args   (:arguments tool-call)
          schema (:input-schema tool)
          errors (when-not (m/validate schema args)
                   (me/humanize (m/explain schema args)))]
      (if-not errors
        args
        (throw (errors/validation-error tool-name args errors))))))
