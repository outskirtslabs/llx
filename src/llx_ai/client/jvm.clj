(ns llx-ai.client.jvm
  (:require
   [babashka.http-client :as http]
   [babashka.json :as json]
   [clojure.string :as str]
   [llx-ai.client :as client]
   [llx-ai.client.runtime :as runtime]
   [llx-ai.errors :as errors]
   [llx-ai.utils.unicode :as unicode])
  (:import
   (java.io IOException)
   (java.net ConnectException SocketTimeoutException UnknownHostException)
   (javax.net.ssl SSLException)))

(set! *warn-on-reflection* true)

(defn- provider-from-url
  [url]
  (try
    (let [host (.getHost (java.net.URI. (str url)))]
      (cond
        (str/includes? host "openai.com") "openai"
        (str/includes? host "anthropic.com") "anthropic"
        (str/includes? host "googleapis.com") "google"
        (str/includes? host "mistral.ai") "mistral"
        :else host))
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
   :stream/run!              runtime/run-stream!
   :registry                 client/default-registry
   :clock/now-ms             (fn [] (System/currentTimeMillis))
   :id/new                   (fn [] (str (java.util.UUID/randomUUID)))
   :env/get                  (fn [k] (System/getenv k))
   :thread/sleep             (fn [ms] (Thread/sleep (long ms)))
   :unicode/sanitize-payload unicode/sanitize-payload})

(defn complete
  ([model context opts]
   (complete (default-env) model context opts))
  ([env model context opts]
   (client/complete env model context opts)))

(defn stream
  ([model context opts]
   (stream (default-env) model context opts))
  ([env model context opts]
   (client/stream env model context opts)))

(defn stream-simple
  ([model context simple-opts]
   (stream-simple (default-env) model context simple-opts))
  ([env model context simple-opts]
   (client/stream-simple env model context simple-opts)))

(defn complete-simple
  ([model context simple-opts]
   (complete-simple (default-env) model context simple-opts))
  ([env model context simple-opts]
   (client/complete-simple env model context simple-opts)))
