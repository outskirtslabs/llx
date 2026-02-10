(ns llx.ai
  (:require
   [llx.ai.impl.client :as impl.client]
   [llx.ai.impl.models :as impl.models]
   [llx.ai.impl.utils.overflow :as impl.overflow]
   [llx.ai.impl.utils.tool-validation :as impl.tool-validation]
   [llx.ai.impl.utils.unicode :as impl.unicode]))

(defn complete
  "Runs a non-streaming completion request and returns a canonical assistant message.

  This public API always requires an explicit `env` argument."
  [env model context opts]
  (impl.client/complete env model context opts))

(defn stream
  "Runs a streaming completion request and returns an LLX event stream.

  This public API always requires an explicit `env` argument."
  [env model context opts]
  (impl.client/stream env model context opts))

(defn complete-simple
  "Runs [[complete]] with normalized simple options.

  This public API always requires an explicit `env` argument."
  [env model context simple-opts]
  (impl.client/complete-simple env model context simple-opts))

(defn stream-simple
  "Runs [[stream]] with normalized simple options.

  This public API always requires an explicit `env` argument."
  [env model context simple-opts]
  (impl.client/stream-simple env model context simple-opts))

(defn get-model
  "Returns the model definition for `provider` and `model-id`, or `nil` when absent."
  [provider model-id]
  (impl.models/get-model provider model-id))

(defn get-models
  "Returns all models for `provider` as a deterministic vector sorted by `:id`."
  [provider]
  (impl.models/get-models provider))

(defn get-providers
  "Returns supported providers as a deterministic sorted vector."
  []
  (impl.models/get-providers))

(defn calculate-cost
  "Calculates usage cost totals from `model` pricing and `usage` token counts."
  [model usage]
  (impl.models/calculate-cost model usage))

(defn supports-xhigh?
  "Returns true when `model` supports `:xhigh` reasoning effort."
  [model]
  (impl.models/supports-xhigh? model))

(defn models-equal?
  "Returns true when two model maps refer to the same `:provider` and `:id`."
  [a b]
  (impl.models/models-equal? a b))

(defn validate-tool-call
  "Validates a `tool-call` entry against matching tool definitions in `tools`."
  [tools tool-call]
  (impl.tool-validation/validate-tool-call tools tool-call))

(defn context-overflow?
  "Returns true when the input matches known context-window overflow patterns."
  ([assistant-message]
   (impl.overflow/context-overflow? assistant-message))
  ([assistant-message context-window]
   (impl.overflow/context-overflow? assistant-message context-window)))

(defn sanitize-surrogates
  "Removes unpaired surrogate code units while preserving valid Unicode pairs."
  [text]
  (impl.unicode/sanitize-surrogates text))
