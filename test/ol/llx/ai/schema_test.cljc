(ns ol.llx.ai.schema-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [deftest is testing]]])
   [ol.llx.ai.impl.schema :as sut]
   [promesa.exec.csp :as sp]))

#?(:clj (set! *warn-on-reflection* true))

(def valid-model
  {:id             "gpt-5"
   :name           "GPT-5"
   :provider       :openai
   :api            :openai-responses
   :base-url       "https://api.openai.com/v1"
   :context-window 400000
   :max-tokens     16000
   :cost           {:input       1.25
                    :output      10.0
                    :cache-read  0.125
                    :cache-write 1.25}
   :capabilities   {:reasoning?      true
                    :input           #{:text :image}
                    :supports-xhigh? true}})

(def valid-usage
  {:input        100
   :output       50
   :cache-read   25
   :cache-write  10
   :total-tokens 185
   :cost         {:input       0.001
                  :output      0.002
                  :cache-read  0.0001
                  :cache-write 0.0002
                  :total       0.0033}})

(def valid-assistant
  {:role        :assistant
   :content     [{:type :text :text "hello"}
                 {:type :thinking :thinking "reasoning"}
                 {:type      :tool-call
                  :id        "call_1"
                  :name      "search"
                  :arguments {:q "foo"}}]
   :api         :openai-responses
   :provider    :openai
   :model       "gpt-5"
   :usage       valid-usage
   :stop-reason :stop
   :timestamp   1730000000000})

(def valid-user
  {:role      :user
   :content   [{:type :text :text "hi"}
               {:type :image :data "base64" :mime-type "image/png"}]
   :timestamp 1730000000001})

(def valid-tool-result
  {:role         :tool-result
   :tool-call-id "call_1"
   :tool-name    "search"
   :content      [{:type :text :text "done"}]
   :is-error?    false
   :timestamp    1730000000002})

(def valid-tool
  {:name         "search"
   :description  "Search utility"
   :input-schema [:map [:q :string]]})

(def valid-adapter
  {:api             :openai-completions
   :build-request   (fn [_env _model _context _opts])
   :open-stream     (fn [_env _request-map])
   :decode-event    (fn [_env _state _raw-chunk])
   :finalize        (fn [_env _state])
   :normalize-error (fn [_env _ex _partial])})

(def valid-env
  {:http/request             (fn [_request-map])
   :json/encode              (fn [_x])
   :json/decode              (fn [_s _opts])
   :clock/now-ms             (fn [] 0)
   :id/new                   (fn [] "id")
   :unicode/sanitize-payload identity})

(deftest model-schema
  (testing "accepts a valid model"
    (is (sut/valid? :ol.llx/model valid-model)))

  (testing "accepts supported openai-completions compat profile keys"
    (is (sut/valid? :ol.llx/model
                    (assoc valid-model
                           :api :openai-completions
                           :compat {:token-field                           :max_tokens
                                    :store?                                false
                                    :supports-usage-stream?                false
                                    :supports-strict-tools?                false
                                    :requires-tool-result-name?            true
                                    :requires-assistant-after-tool-result? false
                                    :requires-thinking-as-text?            true}))))

  (testing "accepts unknown model keys"
    (is (sut/valid? :ol.llx/model (assoc valid-model :unknown true)))))

(deftest usage-schema
  (testing "accepts usage where total-tokens equals component sum"
    (is (sut/valid? :ol.llx/usage valid-usage)))

  (testing "accepts usage where provider total-tokens does not match component sum (Google!)"
    (is (sut/valid? :ol.llx/usage (assoc valid-usage :total-tokens 999)))))

(deftest message-schemas
  (testing "accepts all three canonical message roles"
    (is (sut/valid? :ol.llx/message valid-user))
    (is (sut/valid? :ol.llx/message valid-assistant))
    (is (sut/valid? :ol.llx/message valid-tool-result)))

  (testing "assistant message accepts unknown keys"
    (is (sut/valid? :ol.llx/message-assistant
                    (assoc valid-assistant :extra "nope")))))

(deftest context-schema
  (testing "context is an ordered vector of canonical messages"
    (is (sut/valid? :ol.llx/context [valid-user valid-assistant valid-tool-result])))

  (testing "context-map enforces required :messages and optional envelope keys"
    (is (sut/valid? :ol.llx/context-map
                    {:messages      [valid-user valid-assistant]
                     :system-prompt "You are helpful."
                     :tools         [valid-tool]}))
    (is (sut/valid? :ol.llx/context-map {:messages [valid-user] :unknown true}))
    (is (not (sut/valid? :ol.llx/context-map {:system-prompt "missing messages"})))))

(deftest event-schemas
  (testing "accepts stream terminal events"
    (is (sut/valid? :ol.llx/event {:type :start :meta {:session "x"}}))
    (is (sut/valid? :ol.llx/event {:type :done :assistant-message valid-assistant}))
    (is (sut/valid? :ol.llx/event {:type :error :assistant-message (assoc valid-assistant :stop-reason :error :error-message "boom")})))

  (testing "rejects malformed events"
    (is (not (sut/valid? :ol.llx/event {:type :done})))
    (is (not (sut/valid? :ol.llx/event {:type :text-delta}))))

  (testing "accepts delta events with required payload"
    (is (sut/valid? :ol.llx/event {:type :text-delta :text "chunk"}))
    (is (sut/valid? :ol.llx/event {:type :thinking-delta :thinking "trace"}))
    (is (sut/valid? :ol.llx/event {:type :toolcall-delta :id "call_1" :name "search" :arguments {:q "foo"}}))))

(deftest options-schema
  (let [unified-opts  {:max-tokens         1024
                       :reasoning          :high
                       :reasoning-effort   :high
                       :session-id         "session-unified"
                       :temperature        0.3
                       :top-p              0.95
                       :thinking-budgets   {:high 1234}
                       :api-key            "k"
                       :headers            {"x-trace-id" "abc"}
                       :signal             ::sig
                       :max-retry-delay-ms 2500
                       :metadata           {:request-id "abc"}}
        provider-opts {:tools             [valid-tool]
                       :tool-choice       :auto
                       :reasoning         {:level :high}
                       :cache-control     :short
                       :session-id        "demo-openai-responses"
                       :temperature       0.3
                       :top-p             0.95
                       :max-output-tokens 1024
                       :signal            ::sig
                       :metadata          {:request-id "abc"}
                       :provider-extra    true}
        input-schema  [:map
                       [:path {:description "File path"} :string]
                       [:limit {:optional true :description "Maximum number of results"} :int]]
        json-schema   (sut/malli->json-schema input-schema)]
    (testing "accepts valid unified request options"
      (is (sut/valid? :ol.llx/unified-request-options unified-opts)))

    (testing "accepts unknown unified request option keys"
      (is (sut/valid? :ol.llx/unified-request-options (assoc unified-opts :unknown 1))))

    (testing "accepts valid provider request options including provider-specific keys"
      (is (sut/valid? :ol.llx/provider-request-options provider-opts)))

    (testing "tool input schema field descriptions survive json schema conversion"
      (is (= "File path" (get-in json-schema [:properties :path :description])))
      (is (= "Maximum number of results"
             (get-in json-schema [:properties :limit :description])))
      (is (some #{:path} (:required json-schema)))
      (is (not (some #{:limit} (:required json-schema)))))))

(deftest options-schema-allows-provider-specific-option-keys
  (is (sut/valid? :ol.llx/provider-request-options {:provider-flag true})))

(deftest schema-registry-rebuilds-when-component-schemas-change
  (with-redefs [sut/schemas
                (assoc sut/schemas
                       :ol.llx/unified-request-options
                       [:map
                        [:foo :string]])]
    (is (sut/valid? :ol.llx/unified-request-options {:foo "bar"}))
    (is (not (sut/valid? :ol.llx/unified-request-options {:session-id "still-old"})))))

(deftest config-schema
  (let [cfg {:providers {:openai            {:api-key "k1"}
                         :anthropic         {:api-key "k2"}
                         :google            {:api-key "k3"}
                         :mistral           {:api-key "k4"}
                         :openai-codex      {:api-key "k6"}
                         :openai-compatible {:api-key  "k5"
                                             :base-url "http://localhost:11434/v1"}}
             :models    [valid-model]}]
    (testing "accepts known provider config"
      (is (sut/valid? :ol.llx/library-config cfg)))

    (testing "accepts unknown provider keys"
      (is (sut/valid? :ol.llx/library-config
                      (assoc-in cfg [:providers :bogus] {:api-key "x"}))))))

(deftest oauth-schema-contracts
  (let [valid-credentials {:access     "access-token"
                           :refresh    "refresh-token"
                           :expires    2000
                           :account-id "acc_1"}
        provider          {:id                    "openai-codex"
                           :name                  "OpenAI Codex"
                           :login                 (fn [_callbacks] nil)
                           :refresh-token         (fn [credentials] credentials)
                           :get-api-key           (fn [credentials] (:access credentials))
                           :uses-callback-server? true}]
    (testing "accepts OpenAI Codex provider and API enum values"
      (is (sut/valid? :ol.llx/provider :openai-codex))
      (is (sut/valid? :ol.llx/api :openai-codex-responses)))

    (testing "accepts oauth credential and provider contracts"
      (is (sut/valid? :ol.llx/oauth-credentials valid-credentials))
      (is (sut/valid? :ol.llx/oauth-provider provider)))

    (testing "rejects oauth credential map missing required keys"
      (is (not (sut/valid? :ol.llx/oauth-credentials (dissoc valid-credentials :refresh))))
      (is (not (sut/valid? :ol.llx/oauth-credentials (dissoc valid-credentials :access))))
      (is (not (sut/valid? :ol.llx/oauth-credentials (dissoc valid-credentials :expires)))))

    (testing "rejects oauth provider missing required function slots"
      (is (not (sut/valid? :ol.llx/oauth-provider (dissoc provider :login))))
      (is (not (sut/valid? :ol.llx/oauth-provider (dissoc provider :refresh-token))))
      (is (not (sut/valid? :ol.llx/oauth-provider (dissoc provider :get-api-key)))))))

(deftest adapter-and-env-schema
  (testing "accepts valid adapter and runtime env"
    (is (sut/valid? :ol.llx/adapter valid-adapter))
    (is (sut/valid? :ol.llx/adapter
                    (assoc valid-adapter
                           :normalize-tool-call-id (fn [_id _model _source] "call_1")
                           :transform-options {:id-normalization-profile :openai-completions})))
    (is (sut/valid? :ol.llx/env valid-env)))

  (testing "rejects adapter missing required function slots"
    (is (not (sut/valid? :ol.llx/adapter (dissoc valid-adapter :finalize)))))

  (testing "rejects env missing required function slots"
    (is (not (sut/valid? :ol.llx/env (dissoc valid-env :http/request))))))

(deftest runtime-boundary-schemas
  (let [valid-stream-state          {:model valid-model}
        valid-event-transition      {:state  valid-stream-state
                                     :events [{:type :text-delta :text "chunk"}]}
        valid-finalize-result       {:assistant-message valid-assistant
                                     :events            [{:type :text-end}]}
        valid-http-response         {:status  200
                                     :headers {"content-type" "application/json"}
                                     :body    {:ok true}}
        valid-start-source-input    {:adapter    valid-adapter
                                     :env        valid-env
                                     :model      valid-model
                                     :request    {:method :post :url "https://example.invalid"}
                                     :response   valid-http-response
                                     :payload-ch (sp/chan)
                                     :control-ch (sp/chan)
                                     :cancelled? (fn [] false)}
        valid-start-source-result   {:stop-fn (fn [])}
        valid-run-stream-base-input {:adapter valid-adapter
                                     :env     valid-env
                                     :model   valid-model
                                     :request {:method :post :url "https://example.invalid"}
                                     :out     (sp/chan)
                                     :state*  (atom {:model valid-model})}
        valid-run-stream-input      (assoc valid-run-stream-base-input
                                           :open-stream! (fn [] {:status 200})
                                           :start-source! (fn [_args] {:stop-fn (fn [])}))
        valid-run-stream-result     {:cancel-fn  (fn [])
                                     :done?      (fn [] false)
                                     :payload-ch (sp/chan)
                                     :control-ch (sp/chan)}
        valid-run-stream-args       valid-run-stream-base-input]
    (testing "accepts runtime adapter boundary maps"
      (is (sut/valid? :ol.llx/http-response-map valid-http-response))
      (is (sut/valid? :ol.llx/runtime-stream-state valid-stream-state))
      (is (sut/valid? :ol.llx/runtime-finalize-input valid-stream-state))
      (is (sut/valid? :ol.llx/runtime-finalize-input
                      {:model    valid-model
                       :response valid-http-response}))
      (is (sut/valid? :ol.llx/runtime-normalize-error-partial nil))
      (is (sut/valid? :ol.llx/runtime-normalize-error-partial {:model valid-model}))
      (is (sut/valid? :ol.llx/runtime-normalize-error-partial {:assistant-message valid-assistant}))
      (is (sut/valid? :ol.llx/raw-stream-chunk "{\"type\":\"delta\"}"))
      (is (sut/valid? :ol.llx/raw-stream-chunk {:type :delta}))
      (is (sut/valid? :ol.llx/stream-channel (sp/chan)))
      (is (sut/valid? :ol.llx/registry-entry {:adapter valid-adapter}))
      (is (sut/valid? :ol.llx/registry-map {:ol.llx.registry/adapters {:openai-completions {:adapter valid-adapter}}}))
      (is (sut/valid? :ol.llx/runtime-decode-event-result valid-event-transition))
      (is (sut/valid? :ol.llx/runtime-finalize-result valid-finalize-result))
      (is (sut/valid? :ol.llx/runtime-start-source-input valid-start-source-input))
      (is (sut/valid? :ol.llx/runtime-start-source-result valid-start-source-result))
      (is (sut/valid? :ol.llx/runtime-run-stream-base-input valid-run-stream-base-input))
      (is (sut/valid? :ol.llx/runtime-run-stream-input valid-run-stream-input))
      (is (sut/valid? :ol.llx/runtime-run-stream-result valid-run-stream-result)))

    (testing "rejects malformed runtime boundary maps"
      (is (not (sut/valid? :ol.llx/http-response-map {:status -1})))
      (is (not (sut/valid? :ol.llx/runtime-stream-state {:assistant-message valid-assistant})))
      (is (not (sut/valid? :ol.llx/runtime-finalize-input {:response {:status "200"}})))
      (is (not (sut/valid? :ol.llx/runtime-normalize-error-partial {:model "gpt-5"})))
      (is (not (sut/valid? :ol.llx/raw-stream-chunk 42)))
      (is (not (sut/valid? :ol.llx/stream-channel {:state* (atom {}) :clock/now-ms (fn [] 0)})))
      (is (not (sut/valid? :ol.llx/registry-entry {:adapter valid-adapter :source-id :x :extra true})))
      (is (not (sut/valid? :ol.llx/runtime-decode-event-result {:state {} :events [{:type :toolcall-delta :id "c1" :name "tool"}]})))
      (is (not (sut/valid? :ol.llx/runtime-finalize-result {:assistant-message {:role :assistant} :events []})))
      (is (not (sut/valid? :ol.llx/runtime-start-source-input (dissoc valid-start-source-input :payload-ch))))
      (is (not (sut/valid? :ol.llx/runtime-start-source-result {:stop-fn 42})))
      (is (not (sut/valid? :ol.llx/runtime-run-stream-base-input (assoc valid-run-stream-base-input :out :not-a-chan))))
      (is (not (sut/valid? :ol.llx/runtime-run-stream-input valid-run-stream-base-input)))
      (is (not (sut/valid? :ol.llx/runtime-run-stream-result {:done? (fn [] true)}))))

    (testing "accepts run-stream argument map contract"
      (is (sut/valid? :ol.llx/runtime-run-stream-args valid-run-stream-args)))))
