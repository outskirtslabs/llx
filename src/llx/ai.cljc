(ns llx.ai
  (:require
   [llx.ai.impl.client :as impl.client]
   #?(:clj [llx.ai.impl.client.jvm :as impl.jvm])
   #?(:cljs [llx.ai.impl.client.node :as impl.node])
   [llx.ai.impl.models :as impl.models]
   [llx.ai.impl.utils.overflow :as impl.overflow]
   [llx.ai.impl.utils.tool-validation :as impl.tool-validation]
   [llx.ai.impl.utils.unicode :as impl.unicode]))

(defn default-env
  "Returns the default environment for your platform."
  []
  #?(:cljd (throw (ex-info "CLJS support not yet implemented" {:type :llx/env-required}))
     :cljs (impl.node/default-env)
     :clj (impl.jvm/default-env)))

(defn complete*
  "Runs one non-streaming assistant turn using provider-style options.

  Use `complete*` when you need provider-level control and adapter-specific
  options. This is the provider path.

  `opts` is validated against `:llx/provider-request-options` in
  `llx/src/llx/ai/impl/schema/options.cljc`.

  Adapter `build-request` boundaries also validate adapter-specific schemas:
  `:llx/openai-completions-provider-options`,
  `:llx/openai-responses-provider-options`,
  `:llx/anthropic-provider-options`, and
  `:llx/google-provider-options`.

  Common provider option keys:

  | key                  | description                                                 |
  |----------------------|-------------------------------------------------------------|
  | `:max-output-tokens` | Requested output token cap.                                 |
  | `:temperature`       | Sampling temperature.                                       |
  | `:top-p`             | Nucleus sampling probability.                               |
  | `:reasoning`         | Provider reasoning map (shape depends on adapter).          |
  | `:api-key`           | Provider API key override for this call.                    |
  | `:headers`           | Additional provider request headers.                        |
  | `:signal`            | Abort/cancel signal forwarded to runtime/provider layer.    |
  | `:metadata`          | Request metadata map forwarded to adapter payload builders. |
  | `:registry`          | Per-call adapter registry override.                         |
  | `:max-retries`       | Retry count for transient failures (default `2`).           |

  Returns a promise that resolves to one canonical assistant message map.
  Errors reject the promise with structured LLX exception data.

  Use [[complete]] for the unified API."
  [env model context opts]
  (impl.client/complete* env model context opts))

(defn stream*
  "Runs one streaming assistant turn using provider-style options.

  Use `stream*` when you need provider-level control and adapter-specific
  options. This is the provider path.

  `opts` is validated against `:llx/provider-request-options` in
  `llx/src/llx/ai/impl/schema/options.cljc`, and adapter-specific schemas are
  enforced at each adapter `build-request` boundary.

  Common provider option keys:

  | key                  | description                                                |
  |----------------------|------------------------------------------------------------|
  | `:max-output-tokens` | Requested output token cap.                                |
  | `:temperature`       | Sampling temperature.                                      |
  | `:top-p`             | Nucleus sampling probability.                              |
  | `:reasoning`         | Provider reasoning map (shape depends on adapter).         |
  | `:api-key`           | Provider API key override for this call.                   |
  | `:headers`           | Additional provider request headers.                       |
  | `:signal`            | Abort/cancel signal forwarded to runtime/provider layer.   |
  | `:metadata`          | Request metadata map forwarded to adapter payload builders.|
  | `:registry`          | Per-call adapter registry override.                        |
  | `:max-retries`       | Retry count for transient failures (default `2`).          |

  Returns a Promesa CSP channel emitting canonical LLX event maps.
  Stream completion emits terminal `:done` or `:error` and closes the channel.
  Consumer cancellation is channel closure.

  Use [[stream]] for the unified API."
  [env model context opts]
  (impl.client/stream* env model context opts))

(defn complete
  "Runs one non-streaming assistant turn using the unified options API.

  This is the unified path and is recommended for most callers.
  `opts` is validated against `:llx/unified-request-options` in
  `llx/src/llx/ai/impl/schema/options.cljc`.

  Unified options are normalized and forwarded to provider adapters.
  For example:
  - `:max-tokens` -> provider `:max-output-tokens`
  - `:reasoning` / `:reasoning-effort` -> provider reasoning shape

  Unified option keys:

  | key                 | description                                                       |
  |---------------------|-------------------------------------------------------------------|
  | `:max-tokens`       | Requested output cap. Defaults to `min(model.max-tokens, 32000)`. |
  | `:temperature`      | Sampling temperature.                                             |
  | `:top-p`            | Nucleus sampling probability.                                     |
  | `:reasoning`        | Reasoning level keyword (`:minimal` `:low` `:medium` `:high` `:xhigh`). |
  | `:reasoning-effort` | Alias for `:reasoning`.                                           |
  | `:api-key`          | Provider API key override for this call.                          |
  | `:headers`          | Additional provider request headers.                              |
  | `:signal`           | Abort/cancel signal forwarded to runtime/provider layer.          |
  | `:metadata`         | Request metadata map forwarded to adapter payload builders.       |
  | `:registry`         | Per-call adapter registry override.                               |

  Returns a promise that resolves to one canonical assistant message map.
  Errors reject the promise with structured LLX exception data.

  Use [[complete*]] if you need provider-specific options that are outside the
  unified schema."
  [env model context unified-opts]
  (impl.client/complete env model context unified-opts))

(defn stream
  "Runs one streaming assistant turn using the unified options API.

  This is the unified path and is recommended for most callers.
  `opts` is validated against `:llx/unified-request-options` in
  `llx/src/llx/ai/impl/schema/options.cljc`.

  Unified option keys:

  | key                 | description                                                       |
  |---------------------|-------------------------------------------------------------------|
  | `:max-tokens`       | Requested output cap. Defaults to `min(model.max-tokens, 32000)`. |
  | `:temperature`      | Sampling temperature.                                             |
  | `:top-p`            | Nucleus sampling probability.                                     |
  | `:reasoning`        | Reasoning level keyword (`:minimal` `:low` `:medium` `:high` `:xhigh`). |
  | `:reasoning-effort` | Alias for `:reasoning`.                                           |
  | `:api-key`          | Provider API key override for this call.                          |
  | `:headers`          | Additional provider request headers.                              |
  | `:signal`           | Abort/cancel signal forwarded to runtime/provider layer.          |
  | `:metadata`         | Request metadata map forwarded to adapter payload builders.       |
  | `:registry`         | Per-call adapter registry override.                               |

  Returns a Promesa CSP channel emitting canonical LLX event maps.
  Stream completion emits terminal `:done` or `:error` and closes the channel.
  Consumer cancellation is channel closure.

  Use [[stream*]] for provider-specific option control."
  [env model context unified-opts]
  (impl.client/stream env model context unified-opts))

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
