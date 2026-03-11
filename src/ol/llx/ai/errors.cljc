(ns ol.llx.ai.errors
  (:require
   [ol.llx.ai.impl.errors :as impl.errors]))

(defn llx-error?
  "Returns true when `ex` is a structured LLX error exception."
  [ex]
  (impl.errors/llx-error? ex))

(defn recoverable?
  "Returns true when `ex` is classified as recoverable."
  [ex]
  (impl.errors/recoverable? ex))

(defn rate-limit-error?
  "Returns true when `ex` is a rate-limit error."
  [ex]
  (impl.errors/rate-limit-error? ex))

(defn quota-exceeded-error?
  "Returns true when `ex` is a quota-exceeded error."
  [ex]
  (impl.errors/quota-exceeded-error? ex))

(defn rate-limited-error?
  "Returns true when `ex` is either a rate-limit or quota-exceeded error."
  [ex]
  (impl.errors/rate-limited-error? ex))

(defn timeout-error?
  "Returns true when `ex` is a timeout error."
  [ex]
  (impl.errors/timeout-error? ex))

(defn client-error?
  "Returns true when `ex` is a non-recoverable client-classified error."
  [ex]
  (impl.errors/client-error? ex))

(defn transient-error?
  "Returns true when `ex` is a transient error eligible for retry handling."
  [ex]
  (impl.errors/transient-error? ex))

(defn should-retry?
  "Returns true when retry policy allows another attempt for `ex`.

  Optional keyword arguments include `:max-retries` and `:current-retry`."
  ([ex]
   (impl.errors/should-retry? ex))
  ([ex & opts]
   (apply impl.errors/should-retry? ex opts)))

(defn retry-delay-ms
  "Returns the retry delay in milliseconds for `ex` at `retry-count`."
  [ex retry-count]
  (impl.errors/retry-delay-ms ex retry-count))
