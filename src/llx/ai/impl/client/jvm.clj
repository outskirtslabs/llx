(ns llx.ai.impl.client.jvm
  (:require
   [babashka.http-client :as http]
   [babashka.json :as json]
   [clojure.java.io :as io]
   [com.fulcrologic.guardrails.malli.core :refer [>defn >defn-]]
   [llx.ai.impl.client :as client]
   [llx.ai.impl.client.event-stream :as stream]
   [llx.ai.impl.errors :as errors]
   [llx.ai.impl.schema :as schema]
   [llx.ai.impl.utils.unicode :as unicode]
   [promesa.core :as p]
   [promesa.exec.csp :as sp])
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

(defn- close-stream-body!
  [response]
  (when-let [body (:body response)]
    (try
      (when (instance? java.io.Closeable body)
        (.close ^java.io.Closeable body))
      (catch Exception _))))

(>defn- start-jvm-source!
        [{:keys [response payload-ch cancelled?] :as input}]
        [:llx/runtime-start-source-input => :llx/runtime-start-source-result]
        (schema/assert-valid! :llx/runtime-start-source-input input)
        (let [reader-future* (atom nil)]
          (reset! reader-future*
                  (future
                    (try
                      (with-open [reader (io/reader (:body response))]
                        (loop [remaining (line-seq reader)]
                          (when (and (seq remaining)
                                     (not (cancelled?)))
                            (when-let [payload (client/runtime-data-line->payload (first remaining))]
                              (when-not (boolean (sp/put! payload-ch (stream/payload-msg payload)))
                                (throw (ex-info "Payload channel closed while streaming" {}))))
                            (recur (next remaining)))))
                      (catch java.lang.InterruptedException _
                        nil)
                      (catch Exception ex
                        (sp/put! payload-ch (stream/error-msg ex)))
                      (finally
                        (sp/close payload-ch)))))
          {:stop-fn (fn []
                      (close-stream-body! response)
                      (when-let [f @reader-future*]
                        (future-cancel f))
                      (sp/close payload-ch))}))

(>defn- open-stream-jvm!
        [{:keys [adapter env model request request-opts]}]
        [[:map
          [:adapter :llx/adapter]
          [:env :llx/env]
          [:model :llx/model]
          [:request :llx/adapter-request-map]
          [:request-opts {:optional true} [:maybe :llx/provider-request-options]]]
         => :llx/deferred]
        (p/future
          (stream/await!
           (stream/open-stream-with-retries*
            {:adapter      adapter
             :env          env
             :model        model
             :request      request
             :request-opts request-opts}))))

(>defn run-stream!
       [{:keys [adapter env model request request-opts] :as input}]
       [:llx/runtime-run-stream-base-input => :llx/runtime-run-stream-result]
       (let [response*      (atom nil)
             runtime-result (stream/run-stream!
                             (assoc input
                                    :cancel! (fn []
                                               (close-stream-body! @response*))
                                    :open-stream! (fn []
                                                    (-> (open-stream-jvm! {:adapter      adapter
                                                                           :env          env
                                                                           :model        model
                                                                           :request      request
                                                                           :request-opts request-opts})
                                                        (p/then (fn [response]
                                                                  (reset! response* response)
                                                                  response))))
                                    :start-source! start-jvm-source!))]
         (schema/assert-valid! :llx/runtime-run-stream-result runtime-result)))

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
   :unicode/sanitize-payload unicode/sanitize-payload})
