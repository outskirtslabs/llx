(ns llx-ai.schema-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llx-ai.schema.options :as schema-options]
   [llx-ai.schema :as sut]))

(set! *warn-on-reflection* true)

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
  {:http/request (fn [_request-map])
   :json/encode  (fn [_x])
   :json/decode  (fn [_s _opts])
   :clock/now-ms (fn [] 0)
   :id/new       (fn [] "id")})

(deftest model-schema
  (testing "accepts a valid model"
    (is (sut/valid? :llx/model valid-model)))

  (testing "rejects unknown model keys"
    (is (not (sut/valid? :llx/model (assoc valid-model :unknown true))))))

(deftest usage-schema
  (testing "accepts usage where total-tokens equals component sum"
    (is (sut/valid? :llx/usage valid-usage)))

  (testing "rejects usage where total-tokens does not match components"
    (is (not (sut/valid? :llx/usage (assoc valid-usage :total-tokens 999))))))

(deftest message-schemas
  (testing "accepts all three canonical message roles"
    (is (sut/valid? :llx/message valid-user))
    (is (sut/valid? :llx/message valid-assistant))
    (is (sut/valid? :llx/message valid-tool-result)))

  (testing "assistant message rejects unknown keys"
    (is (not (sut/valid? :llx/message-assistant
                         (assoc valid-assistant :extra "nope"))))))

(deftest context-schema
  (testing "context is an ordered vector of canonical messages"
    (is (sut/valid? :llx/context [valid-user valid-assistant valid-tool-result])))

  (testing "context-map enforces required :messages and optional envelope keys"
    (is (sut/valid? :llx/context-map
                    {:messages      [valid-user valid-assistant]
                     :system-prompt "You are helpful."
                     :tools         [valid-tool]}))
    (is (not (sut/valid? :llx/context-map {:messages [valid-user] :unknown true})))
    (is (not (sut/valid? :llx/context-map {:system-prompt "missing messages"})))))

(deftest event-schemas
  (testing "accepts stream terminal events"
    (is (sut/valid? :llx/event {:type :start :meta {:session "x"}}))
    (is (sut/valid? :llx/event {:type :done :assistant-message valid-assistant}))
    (is (sut/valid? :llx/event {:type :error :assistant-message (assoc valid-assistant :stop-reason :error :error-message "boom")})))

  (testing "rejects malformed events"
    (is (not (sut/valid? :llx/event {:type :done})))
    (is (not (sut/valid? :llx/event {:type :text-delta}))))

  (testing "accepts delta events with required payload"
    (is (sut/valid? :llx/event {:type :text-delta :text "chunk"}))
    (is (sut/valid? :llx/event {:type :thinking-delta :thinking "trace"}))
    (is (sut/valid? :llx/event {:type :toolcall-delta :id "call_1" :name "search" :arguments {:q "foo"}}))))

(deftest options-schema
  (let [opts {:tools             [valid-tool]
              :tool-choice       :auto
              :reasoning         {:level :high}
              :cache-control     :short
              :session-id        "demo-openai-responses"
              :temperature       0.3
              :top-p             0.95
              :max-output-tokens 1024
              :signal            ::sig
              :metadata          {:request-id "abc"}}]
    (testing "accepts valid request options"
      (is (sut/valid? :llx/request-options opts)))

    (testing "rejects unknown request option keys"
      (is (not (sut/valid? :llx/request-options (assoc opts :unknown 1)))))))

(deftest schema-registry-rebuilds-when-component-schemas-change
  (with-redefs [schema-options/schemas
                (assoc schema-options/schemas
                       :llx/request-options
                       [:map {:closed true}
                        [:foo :string]])]
    (is (sut/valid? :llx/request-options {:foo "bar"}))
    (is (not (sut/valid? :llx/request-options {:session-id "still-old"})))))

(deftest config-schema
  (let [cfg {:providers {:openai            {:api-key "k1"}
                         :anthropic         {:api-key "k2"}
                         :google            {:api-key "k3"}
                         :mistral           {:api-key "k4"}
                         :openai-compatible {:api-key  "k5"
                                             :base-url "http://localhost:11434/v1"}}
             :models    [valid-model]}]
    (testing "accepts known provider config"
      (is (sut/valid? :llx/library-config cfg)))

    (testing "rejects unknown provider keys"
      (is (not (sut/valid? :llx/library-config
                           (assoc-in cfg [:providers :bogus] {:api-key "x"})))))))

(deftest adapter-and-env-schema
  (testing "accepts valid adapter and runtime env"
    (is (sut/valid? :llx/adapter valid-adapter))
    (is (sut/valid? :llx/adapter
                    (assoc valid-adapter
                           :normalize-tool-call-id (fn [_id _model _source] "call_1")
                           :transform-options {:id-normalization-profile :openai-completions})))
    (is (sut/valid? :llx/env valid-env)))

  (testing "rejects adapter missing required function slots"
    (is (not (sut/valid? :llx/adapter (dissoc valid-adapter :finalize)))))

  (testing "rejects env missing required function slots"
    (is (not (sut/valid? :llx/env (dissoc valid-env :http/request))))))

(deftest runtime-boundary-schemas
  (let [valid-event-transition {:state  {:cursor 1}
                                :events [{:type :text-delta :text "chunk"}]}
        valid-finalize-result  {:assistant-message valid-assistant
                                :events            [{:type :text-end}]}
        valid-run-stream-args  {:adapter valid-adapter
                                :env     valid-env
                                :model   valid-model
                                :request {:method :post :url "https://example.invalid"}
                                :out     {:queue :q}
                                :state*  (atom {:model valid-model})}]
    (testing "accepts runtime adapter boundary maps"
      (is (sut/valid? :llx/runtime-decode-event-result valid-event-transition))
      (is (sut/valid? :llx/runtime-finalize-result valid-finalize-result)))

    (testing "rejects malformed runtime boundary maps"
      (is (not (sut/valid? :llx/runtime-decode-event-result {:state {} :events [{:type :toolcall-delta :id "c1" :name "tool"}]})))
      (is (not (sut/valid? :llx/runtime-finalize-result {:assistant-message {:role :assistant} :events []}))))

    (testing "accepts run-stream argument map contract"
      (is (sut/valid? :llx/runtime-run-stream-args valid-run-stream-args)))))
