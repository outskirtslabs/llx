(ns llx.ai.impl.client.runtime
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [llx.ai.impl.errors :as errors]
   [llx.ai.impl.schema :as schema]
   [promesa.exec.csp :as sp]
   [taoensso.trove :as trove]))

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

(defn- put-event!
  [out event]
  (boolean (sp/put! out event)))

(defn- close-channel!
  [out]
  (sp/close out)
  nil)

(defn- cancelled?
  [out cancelled*]
  (or @cancelled* (sp/closed? out)))

(defn- emit-events!
  [out cancel-fn events]
  (loop [remaining (seq events)]
    (if-not remaining
      true
      (let [event (first remaining)]
        (schema/assert-valid! :llx/event event)
        (if (put-event! out event)
          (recur (next remaining))
          (do
            (cancel-fn)
            false))))))

(defn- emit-terminal-error!
  [adapter env model out state* stream-ex]
  (let [[assistant-message normalize-failed?] (try
                                                [(schema/assert-valid!
                                                  :llx/message-assistant
                                                  ((:normalize-error adapter) env stream-ex @state*))
                                                 false]
                                                (catch Exception normalize-ex
                                                  [(fallback-assistant-message env model stream-ex normalize-ex)
                                                   true]))]
    (trove/log! {:level :error
                 :id    :llx.obs/stream-event-error
                 :data  {:call-id                 (:call/id env)
                         :provider                (:provider model)
                         :api                     (:api model)
                         :model-id                (:id model)
                         :error-type              (:type (ex-data stream-ex))
                         :error-message           (ex-message stream-ex)
                         :normalize-error-failed? normalize-failed?}})
    (put-event! out {:type :error :assistant-message assistant-message})))

(>defn run-stream!
       [{:keys [adapter env model request out state* request-opts]}]
       [:llx/runtime-run-stream-input => any?]
       (schema/assert-valid! :llx/runtime-run-stream-input
                             {:adapter      adapter
                              :env          env
                              :model        model
                              :request      request
                              :out          out
                              :state*       state*
                              :request-opts request-opts})
       (let [cancelled* (atom false)
             done*      (atom false)
             response*  (atom nil)
             future*    (atom nil)]
         (letfn [(cancel-fn []
                   (when (compare-and-set! cancelled* false true)
                     (when-let [body (:body @response*)]
                       (try
                         (when (instance? java.io.Closeable body)
                           (.close ^java.io.Closeable body))
                         (catch Exception _)))
                     (when-let [f @future*]
                       (future-cancel f))))]
           ;; TODO use virtual thread
           (reset! future*
                   (future
                     (try
                       (let [max-retries (get request-opts :max-retries 2)
                             sleep-fn    (or (:thread/sleep env) (fn [ms] (Thread/sleep (long ms))))
                             response    (errors/retry-loop
                                          #((:open-stream adapter) env model request)
                                          max-retries
                                          sleep-fn
                                          {:call-id  (:call/id env)
                                           :provider (:provider model)})]
                         (reset! response* response)
                         (trove/log! {:level :info
                                      :id    :llx.obs/stream-start
                                      :data  {:call-id  (:call/id env)
                                              :provider (:provider model)
                                              :api      (:api model)
                                              :model-id (:id model)}})
                         (when-not (put-event! out {:type :start})
                           (cancel-fn))
                         (let [item-index* (atom 0)]
                           (with-open [reader (io/reader (:body response))]
                             (doseq [line (line-seq reader)]
                               (when-not (cancelled? out cancelled*)
                                 (when-let [payload (data-line->payload line)]
                                   (let [decoded                (or (when-let [decode-safe (:json/decode-safe env)]
                                                                      (decode-safe payload {:key-fn keyword}))
                                                                    {})
                                         provider-item-type     (or (:type decoded)
                                                                    (when (contains? decoded :candidates) "candidates")
                                                                    (when (contains? decoded :choices) "choices"))
                                         {:keys [state events]} (schema/assert-valid!
                                                                 :llx/runtime-decode-event-result
                                                                 ((:decode-event adapter) env @state* payload))
                                         event-type             (:type (first events))
                                         payload-bytes          (count payload)
                                         idx                    @item-index*]
                                     (swap! item-index* inc)
                                     (trove/log! {:level :trace
                                                  :id    :llx.obs/stream-item-received
                                                  :data  {:call-id            (:call/id env)
                                                          :provider           (:provider model)
                                                          :api                (:api model)
                                                          :model-id           (:id model)
                                                          :item-index         idx
                                                          :provider-item-type provider-item-type
                                                          :llx-event-type     event-type
                                                          :done?              false
                                                          :payload-bytes      payload-bytes}})
                                     (reset! state* state)
                                     (emit-events! out cancel-fn events)))))))
                         (when-not (cancelled? out cancelled*)
                           (let [{:keys [assistant-message events]} (schema/assert-valid!
                                                                     :llx/runtime-finalize-result
                                                                     ((:finalize adapter) env @state*))]
                             (when (emit-events! out cancel-fn events)
                               (schema/assert-valid! :llx/message-assistant assistant-message)
                               (trove/log! {:level :info
                                            :id    :llx.obs/stream-done
                                            :data  {:call-id             (:call/id env)
                                                    :provider            (:provider model)
                                                    :api                 (:api model)
                                                    :model-id            (:id model)
                                                    :stop-reason         (:stop-reason assistant-message)
                                                    :usage               (:usage assistant-message)
                                                    :content-block-count (count (:content assistant-message))}})
                               (when-not (put-event! out {:type :done :assistant-message assistant-message})
                                 (cancel-fn))))))
                       (catch java.lang.InterruptedException _
                         nil)
                       (catch Exception stream-ex
                         (when-not (cancelled? out cancelled*)
                           (emit-terminal-error! adapter env model out state* stream-ex)))
                       (finally
                         (reset! done* true)
                         (close-channel! out)))))
           {:cancel-fn cancel-fn
            :done?     (fn [] (boolean @done*))})))
