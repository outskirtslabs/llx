(ns llx.ai.impl.utils.rate-limit)

(def ^:private rate-limit-patterns
  [#"rate limit"
   #"rate-limit"
   #"too many requests"
   #"quota exceeded"
   #"please retry in \d+(\.\d+)?s"
   #"\b429\b"])

(defn- rate-limit-pattern-match?
  [error-message]
  (boolean
   (and (string? error-message)
        (some #(re-find (re-pattern (str "(?i)" (.pattern %))) error-message)
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
