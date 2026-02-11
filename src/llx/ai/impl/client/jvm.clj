(ns llx.ai.impl.client.jvm
  (:require
   [babashka.http-client :as http]
   [babashka.json :as json]
   [clojure.java.io :as io]
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [llx.ai.impl.client :as client]
   [llx.ai.impl.errors :as errors]
   [llx.ai.impl.schema :as schema]
   [llx.ai.impl.utils.await :as await]
   [llx.ai.impl.utils.unicode :as unicode]
   [promesa.exec.csp :as sp]
   [taoensso.trove :as trove])
  (:import
   (java.io IOException)
   (java.net ConnectException SocketTimeoutException UnknownHostException)
   (javax.net.ssl SSLException)))

(set! *warn-on-reflection* true)

(defn- provider-from-url
  [url]
  (try
    (client/provider-from-host
     (.getHost (java.net.URI. (str url))))
    (catch Exception _
      "unknown")))

(defn- wrapped-http-request
  [request]
  (try
    (http/request request)
    (catch SocketTimeoutException e
      (throw (errors/timeout-error
              (provider-from-url (:url request))
              (or (.getMessage e) "Request timeout"))))
    (catch ConnectException e
      (throw (errors/connection-error
              (provider-from-url (:url request))
              (or (.getMessage e) "Connection refused"))))
    (catch UnknownHostException e
      (throw (errors/connection-error
              (provider-from-url (:url request))
              (str "Unknown host: " (.getMessage e)))))
    (catch SSLException e
      (throw (errors/connection-error
              (provider-from-url (:url request))
              (str "SSL error: " (.getMessage e)))))
    (catch IOException e
      (throw (errors/connection-error
              (provider-from-url (:url request))
              (or (.getMessage e) "I/O error"))))))

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
        (if (boolean (sp/put! out event))
          (recur (next remaining))
          (do
            (cancel-fn)
            false))))))

(defn- emit-terminal-error!
  [adapter env model out state* stream-ex]
  (boolean (sp/put! out (client/runtime-terminal-error-event adapter env model state* stream-ex))))

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
           (reset! future*
                   (future
                     (try
                       (let [max-retries (get request-opts :max-retries 2)
                             sleep-fn    (or (:thread/sleep env) (fn [ms] (Thread/sleep (long ms))))
                             response    (await/await!
                                          (errors/retry-loop-async
                                           (fn []
                                             ((:open-stream adapter) env model request))
                                           max-retries
                                           sleep-fn
                                           {:call-id  (:call/id env)
                                            :provider (:provider model)}))
                             response    (schema/assert-valid! :llx/http-response-map response)]
                         (reset! response* response)
                         (trove/log! {:level :info
                                      :id    :llx.obs/stream-start
                                      :data  {:call-id  (:call/id env)
                                              :provider (:provider model)
                                              :api      (:api model)
                                              :model-id (:id model)}})
                         (when-not (boolean (sp/put! out {:type :start}))
                           (cancel-fn))
                         (let [item-index* (atom 0)]
                           (with-open [reader (io/reader (:body response))]
                             (doseq [line (line-seq reader)]
                               (when-not (cancelled? out cancelled*)
                                 (when-let [payload (client/runtime-data-line->payload line)]
                                   (let [provider-item-type     (client/runtime-payload->provider-item-type env payload)
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
                               (when-not (boolean (sp/put! out {:type :done :assistant-message assistant-message}))
                                 (cancel-fn))))))
                       (catch java.lang.InterruptedException _
                         nil)
                       (catch Exception stream-ex
                         (when-not (cancelled? out cancelled*)
                           (emit-terminal-error! adapter env model out state* stream-ex)))
                       (finally
                         (reset! done* true)
                         (sp/close out)))))
           {:cancel-fn cancel-fn
            :done?     (fn [] (boolean @done*))})))

(defn default-env
  []
  {:http/request             wrapped-http-request
   :json/encode              json/write-str
   :json/decode              (fn [s _opts] (json/read-str s {:key-fn keyword}))
   :json/decode-safe         (fn [s _opts]
                               (try
                                 (json/read-str s {:key-fn keyword})
                                 (catch Exception _
                                   nil)))
   :http/read-body-string    (fn [body] (slurp body))
   :stream/run!              run-stream!
   :registry                 client/default-registry
   :clock/now-ms             (fn [] (System/currentTimeMillis))
   :id/new                   (fn [] (str (java.util.UUID/randomUUID)))
   :env/get                  (fn [k] (System/getenv k))
   :thread/sleep             (fn [ms] (Thread/sleep (long ms)))
   :unicode/sanitize-payload unicode/sanitize-payload})
