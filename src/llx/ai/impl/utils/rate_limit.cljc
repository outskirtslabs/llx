(ns llx.ai.impl.utils.rate-limit)

(def ^:private rate-limit-patterns
  [#"(?i)rate limit"
   #"(?i)rate-limit"
   #"(?i)too many requests"
   #"(?i)quota exceeded"
   #"(?i)please retry in \d+(\.\d+)?s"
   #"(?i)\b429\b"])

(defn- rate-limit-pattern-match?
  [error-message]
  (boolean
   (and (string? error-message)
        (some #(re-find % error-message)
              rate-limit-patterns))))

(defn rate-limited?
  "Returns true when `assistant-message` appears to represent a provider
  quota or rate-limit error."
  [assistant-message]
  (let [stop-reason   (:stop-reason assistant-message)
        error-type    (:error-type assistant-message)
        error-message (:error-message assistant-message)]
    (or (contains? #{:llx/rate-limit :llx/quota-exceeded} error-type)
        (and (= :error stop-reason)
             (rate-limit-pattern-match? error-message)))))
