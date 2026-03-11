(ns ol.llx.ai.impl.oauth.openai-codex
  (:require
   [clojure.string :as str]
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]))

(def token-claim-path
  "https://api.openai.com/auth")

(def default-redirect-uri
  "http://localhost:1455/auth/callback")

(def default-authorize-url
  "https://auth.openai.com/oauth/authorize")

(def default-scope
  "openid profile email offline_access")

(defn- parse-query-pairs
  [query]
  (->> (str/split (or query "") #"&")
       (keep (fn [part]
               (let [[k v] (str/split part #"=" 2)]
                 (when (seq (or k ""))
                   [(str/lower-case k) (or v "")]))))
       (into {})))

(defn- parse-url-query
  [value]
  (try
    #?(:clj
       (let [uri   (java.net.URI. value)
             query (.getQuery uri)]
         (when (seq query)
           (let [pairs (parse-query-pairs query)
                 code  (get pairs "code")
                 state (get pairs "state")]
             (cond-> {}
               (seq code) (assoc :code code)
               (seq state) (assoc :state state)))))
       :cljs
       (let [url   (js/URL. value)
             code  (.get (.-searchParams url) "code")
             state (.get (.-searchParams url) "state")]
         (cond-> {}
           (seq code) (assoc :code code)
           (seq state) (assoc :state state))))
    (catch #?(:clj Exception :cljs :default) _
      nil)))

(>defn parse-authorization-input
       [input]
       [[:maybe :string]
        => [:map
            [:code {:optional true} :string]
            [:state {:optional true} :string]]]
       (let [value (str/trim (or input ""))]
         (cond
           (str/blank? value)
           {}

           :else
           (or (let [parsed (parse-url-query value)]
                 (when (seq (:code parsed))
                   parsed))
               (when (str/includes? value "#")
                 (let [[code state] (str/split value #"#" 2)]
                   (cond-> {}
                     (seq code) (assoc :code code)
                     (seq state) (assoc :state state))))
               (when (str/includes? value "code=")
                 (let [pairs (parse-query-pairs value)
                       code  (get pairs "code")
                       state (get pairs "state")]
                   (cond-> {}
                     (seq code) (assoc :code code)
                     (seq state) (assoc :state state))))
               {:code value}))))

(defn- decode-b64-url
  [payload]
  (let [payload (or payload "")]
    #?(:clj
       (try
         (let [decoded (.decode (java.util.Base64/getUrlDecoder) payload)]
           (String. decoded java.nio.charset.StandardCharsets/UTF_8))
         (catch Exception _
           nil))
       :cljs
       (try
         (let [normalized (-> payload
                              (str/replace "-" "+")
                              (str/replace "_" "/"))
               pad-size   (mod (- 4 (mod (count normalized) 4)) 4)
               padded     (str normalized (apply str (repeat pad-size "=")))]
           (js/atob padded))
         (catch :default _
           nil)))))

(>defn account-id-from-access-token
       [token]
       [[:maybe :string] => [:maybe :string]]
       (when (string? token)
         (let [parts          (str/split token #"\." 3)
               payload-b64    (second parts)
               payload-string (decode-b64-url payload-b64)]
           (when (string? payload-string)
             (second
              (re-find
               #"\"https://api.openai.com/auth\"\s*:\s*\{[^\}]*\"chatgpt_account_id\"\s*:\s*\"([^\"]+)\""
               payload-string))))))

(defn- resolve-authorization-code
  [{:keys [wait-for-code cancel-wait]} on-manual-code-input]
  (if (fn? on-manual-code-input)
    (let [manual-code (on-manual-code-input)]
      (if (seq manual-code)
        (do
          (when (fn? cancel-wait)
            (cancel-wait))
          manual-code)
        (when (fn? wait-for-code)
          (wait-for-code))))
    (when (fn? wait-for-code)
      (wait-for-code))))

(defn- validate-state!
  [expected parsed]
  (let [actual (:state parsed)]
    (when (and (seq actual)
               (not= actual expected))
      (throw (ex-info "State mismatch" {:expected expected :actual actual}))))
  parsed)

(>defn login-openai-codex
       [callbacks hooks]
       [:ol.llx/oauth-login-callbacks :ol.llx/oauth-login-hooks => :ol.llx/oauth-credentials]
       (let [{:keys [on-auth
                     on-prompt
                     on-manual-code-input]}       callbacks
             {:keys [create-authorization-flow
                     start-local-oauth-server
                     exchange-authorization-code
                     account-id-from-access-token
                     redirect-uri]}               hooks
             {:keys [verifier state url]}         (create-authorization-flow)
             server                               (start-local-oauth-server state)
             redirect-uri                         (or redirect-uri default-redirect-uri)]
         (when (fn? on-auth)
           (on-auth {:url          url
                     :instructions "A browser window should open. Complete login to finish."}))
         (try
           (let [initial-input (resolve-authorization-code server on-manual-code-input)
                 input         (if (seq initial-input)
                                 initial-input
                                 (when (fn? on-prompt)
                                   (on-prompt {:message "Paste the authorization code (or full redirect URL):"})))
                 parsed        (->> input
                                    parse-authorization-input
                                    (validate-state! state))
                 code          (:code parsed)
                 _             (when-not (seq code)
                                 (throw (ex-info "Missing authorization code" {})))
                 token-result  (exchange-authorization-code code verifier redirect-uri)
                 success?      (= :success (:type token-result))
                 _             (when-not success?
                                 (throw (ex-info "Token exchange failed" {:token-result token-result})))
                 access-token  (:access token-result)
                 account-id    (account-id-from-access-token access-token)
                 _             (when-not (seq account-id)
                                 (throw (ex-info "Failed to extract account id" {})))]
             {:access     access-token
              :refresh    (:refresh token-result)
              :expires    (:expires token-result)
              :account-id account-id})
           (finally
             (when-let [close-fn (:close server)]
               (close-fn))))))
