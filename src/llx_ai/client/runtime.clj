(ns llx-ai.client.runtime
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [llx-ai.event-stream :as event-stream]))

(defn- data-line->payload
  [line]
  (when (str/starts-with? line "data:")
    (let [payload (str/trim (subs line (count "data:")))]
      (when (and (seq payload) (not= payload "[DONE]"))
        payload))))

(defn run-stream!
  [{:keys [adapter env model request out state*]}]
  (future
    (try
      (let [response ((:open-stream adapter) env model request)]
        (event-stream/push! out {:type :start})
        (with-open [reader (io/reader (:body response))]
          (doseq [line (line-seq reader)]
            (when-let [payload (data-line->payload line)]
              (let [{:keys [state events]} ((:decode-event adapter) env @state* payload)]
                (reset! state* state)
                (doseq [event events]
                  (event-stream/push! out event))))))
        (let [{:keys [assistant-message events]} ((:finalize adapter) env @state*)]
          (doseq [event events]
            (event-stream/push! out event))
          (event-stream/push! out {:type :done :assistant-message assistant-message}))
        (event-stream/end! out))
      (catch Exception ex
        (let [assistant-message ((:normalize-error adapter) env ex @state*)]
          (event-stream/push! out {:type :error :assistant-message assistant-message})
          (event-stream/end! out))))))
