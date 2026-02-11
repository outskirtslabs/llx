(ns llx.ai.impl.client.jvm
  (:require
   [babashka.http-client :as http]
   [babashka.json :as json]
   [clojure.java.io :as io]
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [llx.ai.impl.client :as client]
   [llx.ai.impl.client.stream :as stream]
   [llx.ai.impl.errors :as errors]
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

(defn- start-jvm-source!
  [{:keys [response payload-ch cancelled?]}]
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

(defn- open-stream-jvm!
  [{:keys [adapter env model request request-opts]}]
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
       [:llx/runtime-run-stream-base-input => any?]
       (stream/run-stream!
        (assoc input
               :open-stream! (fn []
                               (open-stream-jvm! {:adapter      adapter
                                                  :env          env
                                                  :model        model
                                                  :request      request
                                                  :request-opts request-opts}))
               :start-source! start-jvm-source!)))

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
