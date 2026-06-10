(ns ol.llx.ai.impl.utils.tool-validation
  (:require
   [ol.llx.ai.impl.errors :as errors]
   [malli.core :as m]
   [malli.error :as me]
   [malli.transform :as mt]))

(def ^:private tool-input-transformer
  (mt/transformer
   (mt/string-transformer)
   (mt/default-value-transformer {::mt/add-optional-keys true})))

(defn- find-tool
  [tools tool-name]
  (some #(when (= (:name %) tool-name) %) tools))

(defn validate-tool-call
  "Validates tool-call arguments against the matching tool `:input-schema`.

  Returns coerced/validated arguments on success.

  Throws `ExceptionInfo` with structured `ex-data`:
  - `{:type :ol.llx/tool-not-found ...}` when `tool-call` name is unknown.
  - `{:type :ol.llx/validation-error ...}` when arguments do not satisfy the schema."
  ([tools tool-call]
   (validate-tool-call nil tools tool-call))
  ([schema-registry tools tool-call]
   (let [tool-name (:name tool-call)
         tool      (find-tool tools tool-name)]
     (when-not tool
       (throw (errors/tool-not-found tool-name (->> tools (mapv :name)))))
     (let [args           (:arguments tool-call)
           schema         (:input-schema tool)
           coerce-options (cond-> {}
                            schema-registry (assoc :registry schema-registry))]
       (try
         (m/coerce schema args tool-input-transformer coerce-options)
         (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) error
           (let [data   (ex-data error)
                 errors (me/humanize (or (get-in data [:data :explain])
                                         (m/explain schema args coerce-options)))]
             (throw (errors/validation-error tool-name args errors)))))))))
