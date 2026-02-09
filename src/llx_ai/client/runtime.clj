(ns llx-ai.client.runtime
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.schema :as schema]))

(set! *warn-on-reflection* true)

(defn- data-line->payload
  [line]
  (when (str/starts-with? line "data:")
    (let [payload (str/trim (subs line (count "data:")))]
      (when (and (seq payload) (not= payload "[DONE]"))
        payload))))

(defn- fallback-error-message
  [stream-ex normalize-ex]
  (str "Stream error: " (or (ex-message stream-ex) (pr-str stream-ex))
       (when normalize-ex
         (str " | normalize-error failure: " (or (ex-message normalize-ex)
                                                 (pr-str normalize-ex))))))

(defn- fallback-assistant-message
  [env model stream-ex normalize-ex]
  {:role          :assistant
   :content       []
   :api           (:api model)
   :provider      (:provider model)
   :model         (:id model)
   :usage         {:input        0
                   :output       0
                   :cache-read   0
                   :cache-write  0
                   :total-tokens 0
                   :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
   :stop-reason   :error
   :error-message (fallback-error-message stream-ex normalize-ex)
   :timestamp     ((:clock/now-ms env))})

(defn- emit-terminal-error!
  [adapter env model out state* stream-ex]
  (let [assistant-message (try
                            (schema/assert-valid!
                             :llx/message-assistant
                             ((:normalize-error adapter) env stream-ex @state*))
                            (catch Exception normalize-ex
                              (fallback-assistant-message env model stream-ex normalize-ex)))]
    (try
      (event-stream/push! out {:type :error :assistant-message assistant-message})
      (finally
        (event-stream/end! out)))))

(>defn run-stream!
       [{:keys [adapter env model request out state*]}]
       [:llx/runtime-run-stream-input => any?]
       (schema/assert-valid! :llx/runtime-run-stream-input
                             {:adapter adapter
                              :env     env
                              :model   model
                              :request request
                              :out     out
                              :state*  state*})
       ;; TODO use virtual thread
       (future
         (try
           (let [response ((:open-stream adapter) env model request)]
             (event-stream/push! out {:type :start})
             (with-open [reader (io/reader (:body response))]
               (doseq [line (line-seq reader)]
                 (when-let [payload (data-line->payload line)]
                   (let [{:keys [state events]} (schema/assert-valid!
                                                 :llx/runtime-decode-event-result
                                                 ((:decode-event adapter) env @state* payload))]
                     (reset! state* state)
                     (doseq [event events]
                       (schema/assert-valid! :llx/event event)
                       (event-stream/push! out event))))))
             (let [{:keys [assistant-message events]} (schema/assert-valid!
                                                       :llx/runtime-finalize-result
                                                       ((:finalize adapter) env @state*))]
               (doseq [event events]
                 (schema/assert-valid! :llx/event event)
                 (event-stream/push! out event))
               (schema/assert-valid! :llx/message-assistant assistant-message)
               (event-stream/push! out {:type :done :assistant-message assistant-message}))
             (event-stream/end! out))
           (catch Exception stream-ex
             (emit-terminal-error! adapter env model out state* stream-ex)))))
