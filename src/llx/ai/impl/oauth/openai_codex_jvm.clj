(ns llx.ai.impl.oauth.openai-codex-jvm
  (:require
   [babashka.http-client :as http]
   [babashka.json :as json]
   [clojure.string :as str]
   [llx.ai.impl.oauth.openai-codex :as oauth])
  (:import
   (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
   (java.net InetSocketAddress URLDecoder URLEncoder)
   (java.nio.charset StandardCharsets)
   (java.security MessageDigest SecureRandom)
   (java.util Base64)))

(set! *warn-on-reflection* true)

(def client-id
  "app_EMoamEEZ73f0CkXaXp7hrann")

(def token-url
  "https://auth.openai.com/oauth/token")

(def ^:private default-originator
  "pi")

(def ^:private callback-timeout-ms
  60000)

(def ^:private success-html
  "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><title>Authentication successful</title></head><body><p>Authentication successful. Return to your terminal to continue.</p></body></html>")

(defn- url-encode
  [s]
  (URLEncoder/encode (str s) (str StandardCharsets/UTF_8)))

(defn- url-decode
  [s]
  (URLDecoder/decode (str s) (str StandardCharsets/UTF_8)))

(defn- form-encode
  [m]
  (->> m
       (map (fn [[k v]]
              (str (url-encode (name k)) "=" (url-encode (str v)))))
       (str/join "&")))

(defn- parse-query
  [query]
  (->> (str/split (or query "") #"&")
       (keep (fn [pair]
               (let [[raw-k raw-v] (str/split pair #"=" 2)
                     k             (some-> raw-k str/lower-case)
                     v             (url-decode (or raw-v ""))]
                 (when (seq (or k ""))
                   [k v]))))
       (into {})))

(defn- b64-url-no-pad
  [^bytes bs]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bs))

(defn- secure-random-bytes
  [n]
  (let [bytes (byte-array n)
        rnd   (SecureRandom.)]
    (.nextBytes rnd bytes)
    bytes))

(defn- random-state
  []
  (let [bytes (secure-random-bytes 16)]
    (->> bytes
         (map (fn [b] (format "%02x" (bit-and (int b) 0xff))))
         (apply str))))

(defn- generate-pkce
  []
  (let [^String verifier      (b64-url-no-pad (secure-random-bytes 64))
        ^MessageDigest digest (MessageDigest/getInstance "SHA-256")
        _                     (.update digest (.getBytes verifier StandardCharsets/UTF_8))
        challenge             (b64-url-no-pad (.digest digest))]
    {:verifier  verifier
     :challenge challenge}))

(defn create-authorization-flow
  ([]
   (create-authorization-flow {}))
  ([{:keys [originator redirect-uri]}]
   (let [{:keys [verifier challenge]} (generate-pkce)
         state                        (random-state)
         redirect-uri                 (or redirect-uri oauth/default-redirect-uri)
         originator                   (or originator default-originator)
         params                       {:response_type              "code"
                                       :client_id                  client-id
                                       :redirect_uri               redirect-uri
                                       :scope                      oauth/default-scope
                                       :code_challenge             challenge
                                       :code_challenge_method      "S256"
                                       :state                      state
                                       :id_token_add_organizations "true"
                                       :codex_cli_simplified_flow  "true"
                                       :originator                 originator}
         url                          (str oauth/default-authorize-url "?" (form-encode params))]
     {:verifier verifier
      :state    state
      :url      url})))

(defn- send-response!
  [^HttpExchange exchange status body]
  (let [bytes (.getBytes (str body) StandardCharsets/UTF_8)]
    (.set (-> exchange .getResponseHeaders) "Content-Type" "text/html; charset=utf-8")
    (.sendResponseHeaders exchange status (alength bytes))
    (with-open [out (.getResponseBody exchange)]
      (.write out bytes))))

(defn start-local-oauth-server
  [expected-state]
  (try
    (let [code*      (atom nil)
          cancelled* (atom false)
          server     (HttpServer/create (InetSocketAddress. "127.0.0.1" 1455) 0)]
      (.createContext
       server
       "/auth/callback"
       (reify HttpHandler
         (handle [_ exchange]
           (try
             (let [query (parse-query (some-> exchange .getRequestURI .getRawQuery))
                   state (get query "state")
                   code  (get query "code")]
               (cond
                 (not= expected-state state)
                 (send-response! exchange 400 "State mismatch")

                 (not (seq code))
                 (send-response! exchange 400 "Missing authorization code")

                 :else
                 (do
                   (reset! code* code)
                   (send-response! exchange 200 success-html))))
             (catch Exception _
               (send-response! exchange 500 "Internal error"))))))
      (.setExecutor server nil)
      (.start server)
      {:close         (fn []
                        (try
                          (.stop server 0)
                          (catch Exception _
                            nil)))
       :cancel-wait   (fn []
                        (reset! cancelled* true))
       :wait-for-code (fn []
                        (loop [remaining-ms callback-timeout-ms]
                          (cond
                            (seq @code*) @code*
                            @cancelled* nil
                            (<= remaining-ms 0) nil
                            :else (do
                                    (Thread/sleep 100)
                                    (recur (- remaining-ms 100))))))})
    (catch Exception _
      {:close         (fn [] nil)
       :cancel-wait   (fn [] nil)
       :wait-for-code (fn [] nil)})))

(defn- parse-json-safe
  [s]
  (try
    (json/read-str (or s "") {:key-fn keyword})
    (catch Exception _
      nil)))

(defn- response->string
  [response]
  (let [body (:body response)]
    (cond
      (string? body) body
      (nil? body) ""
      :else (slurp body))))

(defn- token-response
  [form]
  (try
    (let [response (http/request {:method  :post
                                  :url     token-url
                                  :headers {"Content-Type" "application/x-www-form-urlencoded"}
                                  :body    (form-encode form)
                                  :throw   false})
          status   (long (or (:status response) 0))
          payload  (parse-json-safe (response->string response))]
      (if (and (<= 200 status 299)
               (seq (:access_token payload))
               (seq (:refresh_token payload))
               (number? (:expires_in payload)))
        {:type    :success
         :access  (:access_token payload)
         :refresh (:refresh_token payload)
         :expires (+ (System/currentTimeMillis)
                     (* 1000 (long (:expires_in payload))))}
        {:type :failed}))
    (catch Exception _
      {:type :failed})))

(defn exchange-authorization-code
  ([code verifier]
   (exchange-authorization-code code verifier oauth/default-redirect-uri))
  ([code verifier redirect-uri]
   (token-response {:grant_type    "authorization_code"
                    :client_id     client-id
                    :code          code
                    :code_verifier verifier
                    :redirect_uri  redirect-uri})))

(defn refresh-openai-codex-token
  [refresh-token]
  (let [result (token-response {:grant_type    "refresh_token"
                                :refresh_token refresh-token
                                :client_id     client-id})]
    (when-not (= :success (:type result))
      (throw (ex-info "Failed to refresh OpenAI Codex token" {})))
    (let [account-id (oauth/account-id-from-access-token (:access result))]
      (when-not (seq account-id)
        (throw (ex-info "Failed to extract account id from token" {})))
      {:access     (:access result)
       :refresh    (:refresh result)
       :expires    (:expires result)
       :account-id account-id})))

(def oauth-provider
  {:id                    "openai-codex"
   :name                  "ChatGPT Plus/Pro (Codex Subscription)"
   :uses-callback-server? true
   :login                 (fn [callbacks]
                            (oauth/login-openai-codex
                             callbacks
                             {:create-authorization-flow    create-authorization-flow
                              :start-local-oauth-server     start-local-oauth-server
                              :exchange-authorization-code  exchange-authorization-code
                              :account-id-from-access-token oauth/account-id-from-access-token
                              :redirect-uri                 oauth/default-redirect-uri}))
   :refresh-token         (fn [credentials]
                            (refresh-openai-codex-token (:refresh credentials)))
   :get-api-key           (fn [credentials]
                            (:access credentials))})
