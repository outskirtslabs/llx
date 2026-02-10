(ns llx-ai.schema
  (:require
   [com.fulcrologic.guardrails.malli.registry :as gr.reg]
   [malli.core :as m]
   [malli.error :as me]
   [malli.json-schema :as mjs]
   [llx-ai.schema.config :as config]
   [llx-ai.schema.core :as core]
   [llx-ai.schema.event :as event]
   [llx-ai.schema.message :as message]
   [llx-ai.schema.model :as model]
   [llx-ai.schema.options :as options]
   [llx-ai.schema.runtime :as runtime]))

(defn custom-schemas
  []
  (merge core/schemas
         model/schemas
         message/schemas
         event/schemas
         options/schemas
         config/schemas
         runtime/schemas))

(defn registry
  []
  (merge (m/default-schemas)
         (custom-schemas)))

(gr.reg/merge-schemas! (custom-schemas))

(defn schema
  [schema-id]
  (m/schema schema-id {:registry (registry)}))

(defn valid?
  [schema-id data]
  (m/validate (schema schema-id) data))

(defn explain
  [schema-id data]
  (m/explain (schema schema-id) data))

(defn humanize
  [schema-id data]
  (me/humanize (explain schema-id data)))

(defn assert-valid!
  [schema-id data]
  (if (valid? schema-id data)
    data
    (throw
     (ex-info "Schema validation failed"
              {:schema schema-id
               :errors (humanize schema-id data)
               :data   data}))))

(defn malli->json-schema
  "Converts a Malli schema form to a JSON Schema object map."
  [schema-form]
  (if (map? schema-form)
    schema-form
    (mjs/transform schema-form)))
