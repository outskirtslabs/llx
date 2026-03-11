(ns ol.llx.ai.impl.utils.overflow)

(def ^:private overflow-patterns
  [#"(?i)prompt is too long"
   #"(?i)input is too long for requested model"
   #"(?i)exceeds the context window"
   #"(?i)input token count.*exceeds the maximum"
   #"(?i)maximum prompt length is \d+"
   #"(?i)reduce the length of the messages"
   #"(?i)maximum context length is \d+ tokens"
   #"(?i)exceeds the limit of \d+"
   #"(?i)exceeds the available context size"
   #"(?i)greater than the context length"
   #"(?i)context window exceeds limit"
   #"(?i)exceeded model token limit"
   #"(?i)context[_ ]length[_ ]exceeded"
   #"(?i)too many tokens"
   #"(?i)token limit exceeded"])

(defn- overflow-status-no-body?
  [error-message]
  (boolean
   (and (string? error-message)
        (re-find #"(?i)^4(00|13)\s*(status code)?\s*\(no body\)" error-message))))

(defn- overflow-pattern-match?
  [error-message]
  (boolean
   (and (string? error-message)
        (some #(re-find % error-message) overflow-patterns))))

(defn context-overflow?
  "Returns true when `assistant-message` appears to be a context-window overflow.

  Supports two detection modes:
  1. error-message pattern matching for explicit overflow responses.
  2. silent overflow heuristic when `context-window` is provided and
     `(:usage :input + :cache-read)` exceeds it."
  ([assistant-message]
   (context-overflow? assistant-message nil))
  ([assistant-message context-window]
   (let [stop-reason   (:stop-reason assistant-message)
         error-message (:error-message assistant-message)
         usage         (:usage assistant-message)
         input-tokens  (+ (long (or (:input usage) 0))
                          (long (or (:cache-read usage) 0)))]
     (or (and (= :error stop-reason)
              (or (overflow-pattern-match? error-message)
                  (overflow-status-no-body? error-message)))
         (and (int? context-window)
              (= :stop stop-reason)
              (> input-tokens context-window))))))
