(ns llx.agent.demo
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [llx.agent :as agent]
   [llx.ai :as ai]
   [llx.ai.impl.oauth.openai-codex :as openai-codex]
   [llx.ai.impl.oauth.openai-codex-jvm :as openai-codex-jvm]
   [llx.ai.oauth :as oauth]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(set! *warn-on-reflection* true)

(def tap-log-file
  "tmp/llx-agent-demo.tap.edn")

(def auth-file
  "tmp/llx-agent-demo-auth.edn")

(def codex-provider-id
  "openai-codex")

(def codex-model-id
  "gpt-5.2-codex")

(defonce pending-codex-login* (atom nil))

(defn- append-line!
  [path value]
  (io/make-parents path)
  (spit path (str (pr-str value) "\n") :append true))

(defn make-file-tap-handler
  "Returns a tap handler that appends each tapped value as one EDN line."
  [path]
  (fn [value]
    (append-line! path value)))

(defn- read-auth-store
  [path]
  (if (.exists (io/file path))
    (let [loaded (edn/read-string (slurp path))]
      (if (map? loaded) loaded {}))
    {}))

(defn- write-auth-store!
  [path store]
  (io/make-parents path)
  (spit path (pr-str store)))

(defn- prompt-line
  [message]
  (println message)
  (flush)
  (if (System/console)
    (or (read-line) "")
    "")) ; nREPL/editor eval sessions often have no interactive stdin

(defn- codex-login-callbacks
  []
  {:on-auth              (fn [{:keys [url instructions]}]
                           (when (seq instructions)
                             (println instructions))
                           (println)
                           (println "Open this URL in your browser:")
                           (println url)
                           (println))
   :on-prompt            (fn [{:keys [message]}]
                           (prompt-line message))
   :on-manual-code-input (fn []
                           (prompt-line "Manual fallback: paste authorization code or callback URL (or press Enter to keep waiting)."))})

(defn- auth-code-from-manual-input
  [state manual-input]
  (let [parsed (openai-codex/parse-authorization-input manual-input)]
    (when (and (seq (:state parsed))
               (not= state (:state parsed)))
      (throw (ex-info "State mismatch" {:expected state :actual (:state parsed)})))
    (:code parsed)))

(defn step-1-start-openai-codex-login!
  "Starts OpenAI Codex OAuth login and returns immediately with URL/state.
   Use `step-1-finish-openai-codex-login!` to complete and persist credentials."
  ([] (step-1-start-openai-codex-login! auth-file))
  ([path]
   (when @pending-codex-login*
     (throw (ex-info "OpenAI Codex OAuth login already pending. Finish or cancel it first."
                     {:pending @pending-codex-login*})))
   (let [{:keys [verifier state url]} (openai-codex-jvm/create-authorization-flow)
         server                       (openai-codex-jvm/start-local-oauth-server state)
         pending                      {:auth-file path
                                       :verifier  verifier
                                       :state     state
                                       :url       url
                                       :server    server}]
     (reset! pending-codex-login* pending)
     (println "Open this URL in your browser:")
     (println url)
     (println)
     (println "After browser login completes, run:")
     (println "  (demo/step-1-finish-openai-codex-login!)")
     (println "Or with manual paste:")
     (println "  (demo/step-1-finish-openai-codex-login! \"<callback-url-or-code>\")")
     {:status :pending
      :url    url
      :state  state})))

(defn step-1-finish-openai-codex-login!
  "Completes a pending OpenAI Codex OAuth login and persists credentials.

   - Without `manual-input`, waits for localhost callback capture.
   - With `manual-input`, parses callback URL/query/code directly."
  ([] (step-1-finish-openai-codex-login! nil))
  ([manual-input]
   (let [{:keys [auth-file verifier state server]} @pending-codex-login*]
     (when-not (map? @pending-codex-login*)
       (throw (ex-info "No pending OpenAI Codex OAuth login. Run step-1-start-openai-codex-login! first."
                       {})))
     (try
       (let [code         (if (seq manual-input)
                            (auth-code-from-manual-input state manual-input)
                            ((:wait-for-code server)))
             _            (when-not (seq code)
                            (throw (ex-info "Missing authorization code" {})))
             token-result (openai-codex-jvm/exchange-authorization-code code verifier)
             _            (when-not (= :success (:type token-result))
                            (throw (ex-info "Token exchange failed" {:token-result token-result})))
             account-id   (openai-codex/account-id-from-access-token (:access token-result))
             _            (when-not (seq account-id)
                            (throw (ex-info "Failed to extract account id from token" {})))
             credentials  {:access     (:access token-result)
                           :refresh    (:refresh token-result)
                           :expires    (:expires token-result)
                           :account-id account-id}
             store        (assoc (read-auth-store auth-file) codex-provider-id credentials)]
         (write-auth-store! auth-file store)
         credentials)
       (finally
         (when-let [close-fn (:close server)]
           (close-fn))
         (reset! pending-codex-login* nil))))))

(defn step-1-cancel-openai-codex-login!
  "Cancels and closes any pending OpenAI Codex OAuth login."
  []
  (when-let [pending @pending-codex-login*]
    (when-let [cancel-wait (:cancel-wait (:server pending))]
      (cancel-wait))
    (when-let [close-fn (:close (:server pending))]
      (close-fn)))
  (reset! pending-codex-login* nil)
  true)

(defn step-1-login-openai-codex!
  "Interactive one-call login helper.
   In non-interactive REPL/editor sessions this starts a pending login and returns immediately."
  ([] (step-1-login-openai-codex! auth-file))
  ([path]
   (if (System/console)
     (let [provider (oauth/get-oauth-provider codex-provider-id)]
       (when-not provider
         (throw (ex-info "OAuth provider is not registered" {:provider-id codex-provider-id})))
       (let [credentials ((:login provider) (codex-login-callbacks))
             store       (assoc (read-auth-store path) codex-provider-id credentials)]
         (write-auth-store! path store)
         credentials))
     (step-1-start-openai-codex-login! path))))

(defn- resolve-oauth-api-key!
  [provider-id path]
  (let [store  (read-auth-store path)
        result (oauth/get-oauth-api-key provider-id store)]
    (when-not result
      (throw (ex-info "No OAuth credentials found. Run step-1-login-openai-codex! first."
                      {:provider-id provider-id
                       :auth-file   path})))
    (let [new-store (assoc store provider-id (:new-credentials result))]
      (when (not= store new-store)
        (write-auth-store! path new-store)))
    (:api-key result)))

(defn make-oauth-api-key-resolver
  "Returns an `llx.agent` `:get-api-key` callback backed by local OAuth creds."
  ([] (make-oauth-api-key-resolver auth-file))
  ([path]
   (fn [provider-name]
     (when (= codex-provider-id provider-name)
       (resolve-oauth-api-key! codex-provider-id path)))))

(defn- start-event-forwarder!
  [runtime]
  (let [events> (agent/subscribe runtime)
        worker  (future
                  (loop []
                    (when-some [event (p/await (sp/take events>))]
                      (tap> event)
                      (recur))))]
    {:events> events>
     :worker  worker}))

(defn- stop-event-forwarder!
  [runtime {:keys [events> worker]}]
  (agent/unsubscribe runtime events>)
  (deref worker 1000 nil)
  nil)

(def read-file-tool
  {:name         "read_file"
   :description  "Read a UTF-8 file from disk and return its contents."
   :input-schema [:map
                  [:path
                   {:description "Absolute or relative file path to read."}
                   :string]]
   :execute      (fn [_tool-call-id {:keys [path]} _abort-signal on-update]
                   (on-update {:stage :reading :path path})
                   {:content [{:type :text
                               :text (slurp path)}]})})

(defn step-2-create-runtime
  "Creates a runtime configured for OpenAI Codex OAuth via `:get-api-key`."
  ([] (step-2-create-runtime auth-file))
  ([path]
   (let [model (ai/get-model :openai-codex codex-model-id)]
     (when-not model
       (throw (ex-info "Codex model not found" {:provider :openai-codex
                                                :model-id codex-model-id})))
     (agent/create-agent {:tools          [read-file-tool]
                          :system-prompt  "Use tools when needed and keep answers concise."
                          :model          model
                          :thinking-level :medium
                          :session-id     "llx-agent-demo-codex-sso"
                          :get-api-key    (make-oauth-api-key-resolver path)}))))

(defn step-3-run-prompt!
  "Runs one prompt against the agent runtime."
  [runtime]
  (p/await
   (agent/prompt runtime
                 [{:role      :user
                   :content   "Use the read_file tool to read deps.edn, then summarize it in one sentence."
                   :timestamp (System/currentTimeMillis)}])))

(defn step-4-close!
  "Stops event forwarding and closes runtime."
  [runtime forwarder]
  (stop-event-forwarder! runtime forwarder)
  (p/await (agent/close runtime))
  true)

(defn run-step-by-step-demo!
  "Runs steps 2-4 assuming OAuth credentials are already stored."
  ([] (run-step-by-step-demo! auth-file))
  ([path]
   (let [runtime   (step-2-create-runtime path)
         forwarder (start-event-forwarder! runtime)]
     (try
       (step-3-run-prompt! runtime)
       (-> runtime agent/state :messages)
       (finally
         (step-4-close! runtime forwarder))))))

(comment
  ;; Evaluate in REPL:
  ;; (require '[llx.agent.demo :as demo] :reload)

  ;; Optional event tap setup.
  (def tap-handler (make-file-tap-handler tap-log-file))
  (io/make-parents tap-log-file)
  (spit tap-log-file "")
  (add-tap tap-handler)

  ;; Local file used by the tool.
  (spit "tmp/llx-agent-demo-input.txt"
        "The build succeeded.\nThe next step is to ship it.\n")

  ;; Step 1: Sign in once and persist OAuth credentials.
  ;; Terminal REPL (interactive): one call
  ;; (step-1-login-openai-codex!)
  ;;
  ;; nREPL/editor (non-interactive): split start/finish
  (step-1-start-openai-codex-login!)
  ;; ... complete browser auth ...
  (step-1-finish-openai-codex-login!)
  ;; Optional cancel/reset:
  (step-1-cancel-openai-codex-login!)

  ;; Step 2: Create runtime that resolves API key from OAuth credentials.
  (def runtime (step-2-create-runtime))
  (def forwarder (start-event-forwarder! runtime))

  ;; Step 3: Run one prompt.
  (step-3-run-prompt! runtime)

  (-> runtime agent/state :messages)

  ;; Step 4: Cleanup.
  (step-4-close! runtime forwarder)

  ;; Convenience path after login:
  ;; (run-step-by-step-demo!)

  ;;
  )
