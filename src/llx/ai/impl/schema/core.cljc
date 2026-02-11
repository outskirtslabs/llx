(ns llx.ai.impl.schema.core
  (:require
   [clojure.string :as str]
   [promesa.core :as p]))

(defn non-blank-string?
  [s]
  (and (string? s) (not (str/blank? s))))

(defn non-negative-int?
  [n]
  (and (int? n) (<= 0 n)))

(defn non-negative-number?
  [n]
  (and (number? n) (<= 0 n)))

(defn deferred?
  [x]
  (p/deferred? x))

(def schemas
  {:llx/id-string       [:and :string [:fn non-blank-string?]]
   :llx/non-neg-int     [:fn non-negative-int?]
   :llx/non-neg-number  [:fn non-negative-number?]
   :llx/provider        [:enum :openai :anthropic :google :mistral :openai-compatible]
   :llx/api             [:enum :openai-responses :openai-completions :anthropic-messages :google-generative-ai]
   :llx/role            [:enum :user :assistant :tool-result]
   :llx/stop-reason     [:enum :stop :length :tool-use :error :aborted]
   :llx/reasoning-level [:enum :minimal :low :medium :high :xhigh]
   :llx/cache-control   [:enum :none :short :long]
   :llx/timestamp-ms    :llx/non-neg-int
   :llx/fn              [:fn ifn?]
   :llx/deferred        [:fn deferred?]
   :llx/metadata-key    [:or :keyword :string]
   :llx/metadata-map    [:map-of :llx/metadata-key :any]})
