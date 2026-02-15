(ns llx.ai.adapters.anthropic-messages-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]]
       :cljs [[cljs.test :refer-macros [deftest is]]])
   [clojure.string :as str]
   [llx.ai.impl.adapters.anthropic-messages :as sut]
   [llx.ai.impl.utils.unicode :as unicode]
   [llx.ai.test-util :as util]))

#?(:clj (set! *warn-on-reflection* true))

(def anthropic-model
  {:id             "claude-sonnet-4-5"
   :name           "Claude Sonnet 4.5"
   :provider       :anthropic
   :api            :anthropic-messages
   :base-url       "https://api.anthropic.com"
   :context-window 200000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text :image}}})

(def priced-anthropic-model
  (assoc anthropic-model :cost {:input 1000.0 :output 2000.0 :cache-read 3000.0 :cache-write 4000.0}))

(defn- fixture
  [name]
  (util/read-edn-file (str "test/llx/ai/fixtures/anthropic/" name ".edn")))

(defn- stub-env
  []
  {:http/request             (fn [_] {:status 200 :body "{}"})
   :json/encode              util/json-write
   :json/decode              (fn [s _opts] (util/json-read s {:key-fn keyword}))
   :json/decode-safe         (fn [s _opts]
                               (util/json-read-safe s {:key-fn keyword}))
   :clock/now-ms             (fn [] 1730000000000)
   :id/new                   (fn [] "id-1")
   :unicode/sanitize-payload unicode/sanitize-payload})

(deftest build-request-converts-canonical-context-to-anthropic-payload
  (let [context  (fixture "request_context")
        request  (sut/build-request
                  (stub-env)
                  anthropic-model
                  context
                  {:api-key "anthropic-key" :max-output-tokens 256}
                  false)
        payload  (util/json-read (:body request) {:key-fn keyword})
        messages (:messages payload)]
    (is (= "https://api.anthropic.com/v1/messages" (:url request)))
    (is (= "anthropic-key" (get-in request [:headers "x-api-key"])))
    (is (= "2023-06-01" (get-in request [:headers "anthropic-version"])))
    (is (= false (:stream payload)))
    (is (= 256 (:max_tokens payload)))
    (is (= "Please solve this."
           (get-in messages [0 :content 0 :text])))
    (is (= "image"
           (get-in messages [0 :content 1 :type])))
    (is (= "internal without signature"
           (get-in messages [1 :content 1 :text])))
    (is (= "thinking"
           (get-in messages [1 :content 2 :type])))
    (is (= "sig_123"
           (get-in messages [1 :content 2 :signature])))
    (is (= "tool_use"
           (get-in messages [1 :content 3 :type])))
    (is (= "tool_result"
           (get-in messages [2 :content 0 :type])))
    (is (= "tool_result"
           (get-in messages [2 :content 1 :type])))
    (is (= 2 (count (get-in messages [2 :content]))))
    (is (= "continue"
           (get-in messages [3 :content])))))

(deftest build-request-emits-provider-payload-trove-signal
  (util/with-captured-logs!
    (fn [logs*]
      (let [context {:messages [{:role :user :content "hello" :timestamp 1}]}
            request (sut/build-request (stub-env) anthropic-model context {:api-key "k"} false)
            payload (util/json-read (:body request) {:key-fn keyword})
            event   (util/first-event logs* :llx.obs/provider-payload)]
        (is (util/submap?
             {:id    :llx.obs/provider-payload
              :level :trace
              :data  {:provider :anthropic
                      :api      :anthropic-messages
                      :model-id "claude-sonnet-4-5"
                      :stream?  false
                      :url      "https://api.anthropic.com/v1/messages"
                      :payload  payload}}
             (util/strip-generated event [:call-id])))))))

(deftest normalize-tool-call-id-sanitizes-and-truncates
  (let [source-id (str "call|bad/id?chars+=" (apply str (repeat 120 "z")))
        out       (sut/normalize-tool-call-id source-id anthropic-model {})]
    (is (<= (count out) 64))
    (is (re-matches #"[A-Za-z0-9_-]+" out))
    (is (not= source-id out))))

(deftest finalize-maps-anthropic-stop-reasons
  (doseq [[input expected] [["end_turn" :stop]
                            ["max_tokens" :length]
                            ["tool_use" :tool-use]
                            ["pause_turn" :stop]
                            ["stop_sequence" :stop]
                            ["refusal" :error]
                            ["sensitive" :error]]]
    (let [result (sut/finalize
                  (stub-env)
                  {:model    anthropic-model
                   :response {:status 200
                              :body   (util/json-write
                                       {:content     [{:type "text" :text "ok"}]
                                        :stop_reason input
                                        :usage       {:input_tokens 1 :output_tokens 2}})}})]
      (is (= expected
             (get-in result [:assistant-message :stop-reason]))))))

(deftest finalize-throws-on-unknown-stop-reason
  (is (thrown-with-msg?
       #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
       #"Unknown Anthropic stop reason"
       (sut/finalize
        (stub-env)
        {:model    anthropic-model
         :response {:status 200
                    :body   (util/json-write
                             {:content     [{:type "text" :text "ok"}]
                              :stop_reason "brand_new_reason"
                              :usage       {:input_tokens 1 :output_tokens 2}})}}))))

(deftest image-only-content-does-not-inject-placeholder-text
  (let [context {:messages [{:role      :user
                             :content   [{:type :image :data "aGVsbG8=" :mime-type "image/png"}]
                             :timestamp 1}
                            {:role         :tool-result
                             :tool-call-id "toolu_img"
                             :tool-name    "vision"
                             :content      [{:type :image :data "aGVsbG8=" :mime-type "image/png"}]
                             :is-error?    false
                             :timestamp    2}]}
        request (sut/build-request (stub-env) anthropic-model context {:api-key "anthropic-key"} false)
        payload (util/json-read (:body request) {:key-fn keyword})]
    (is (= "image" (get-in payload [:messages 0 :content 0 :type])))
    (is (= 1 (count (get-in payload [:messages 0 :content]))))
    (is (= "tool_result" (get-in payload [:messages 1 :content 0 :type])))
    (is (= "image" (get-in payload [:messages 1 :content 0 :content 0 :type])))))

(def anthropic-opus-model
  {:id             "claude-opus-4-6-20250801"
   :name           "Claude Opus 4.6"
   :provider       :anthropic
   :api            :anthropic-messages
   :base-url       "https://api.anthropic.com"
   :context-window 200000
   :max-tokens     32000
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text :image}}})

(deftest build-request-enables-adaptive-thinking-for-opus-4-6
  (let [context {:messages [{:role :user :content "think about this" :timestamp 1}]}
        request (sut/build-request (stub-env) anthropic-opus-model context
                                   {:api-key "k" :reasoning {:level :medium}} false)
        payload (util/json-read (:body request) {:key-fn keyword})]
    (is (= {:thinking      {:type "adaptive"}
            :output_config {:effort "medium"}
            :max_tokens    10666
            :model         "claude-opus-4-6-20250801"
            :stream        false}
           (select-keys payload [:thinking :output_config :max_tokens :model :stream])))))

(deftest build-request-enables-budget-thinking-for-older-reasoning-model
  (let [context {:messages [{:role :user :content "think about this" :timestamp 1}]}
        request (sut/build-request (stub-env) anthropic-model context
                                   {:api-key "k" :reasoning {:level :medium}} false)
        payload (util/json-read (:body request) {:key-fn keyword})]
    (is (= {:thinking   {:type "enabled" :budget_tokens 7168}
            :max_tokens 8192
            :model      "claude-sonnet-4-5"
            :stream     false}
           (select-keys payload [:thinking :output_config :max_tokens :model :stream])))))

(deftest build-request-uses-custom-thinking-budgets
  (let [context {:messages [{:role :user :content "think about this" :timestamp 1}]}
        request (sut/build-request (stub-env) anthropic-model context
                                   {:api-key          "k"
                                    :reasoning        {:level :medium}
                                    :thinking-budgets {:medium 2048}} false)
        payload (util/json-read (:body request) {:key-fn keyword})]
    (is (= {:thinking   {:type "enabled" :budget_tokens 2048}
            :max_tokens 4778
            :model      "claude-sonnet-4-5"
            :stream     false}
           (select-keys payload [:thinking :output_config :max_tokens :model :stream])))))

(deftest build-request-omits-thinking-when-no-reasoning-opts
  (let [context {:messages [{:role :user :content "just chat" :timestamp 1}]}
        request (sut/build-request (stub-env) anthropic-model context
                                   {:api-key "k"} false)
        payload (util/json-read (:body request) {:key-fn keyword})]
    (is (= {:max_tokens 2730 :model "claude-sonnet-4-5" :stream false}
           (select-keys payload [:thinking :output_config :max_tokens :model :stream])))))

(deftest build-request-omits-thinking-when-model-not-reasoning
  (let [non-reasoning-model (assoc-in anthropic-model [:capabilities :reasoning?] false)
        context             {:messages [{:role :user :content "chat" :timestamp 1}]}
        request             (sut/build-request (stub-env) non-reasoning-model context
                                               {:api-key "k" :reasoning {:level :high}} false)
        payload             (util/json-read (:body request) {:key-fn keyword})]
    (is (= {:max_tokens 2730 :model "claude-sonnet-4-5" :stream false}
           (select-keys payload [:thinking :output_config :max_tokens :model :stream])))))

(deftest decode-event-stream-contract
  (let [env                                                                   (stub-env)
        chunks                                                                (fixture "stream_events")
        init-state                                                            {:model anthropic-model}
        {:keys [state events]}
        (reduce (fn [{:keys [state events]} chunk]
                  (let [{next-state :state next-events :events}
                        (sut/decode-event env state (util/json-write chunk))]
                    {:state  next-state
                     :events (into events next-events)}))
                {:state init-state :events []}
                chunks)
        finalize-result                                                       (sut/finalize env state)]
    (is (= [:text-start
            :text-delta
            :text-delta
            :text-end
            :thinking-start
            :thinking-delta
            :thinking-end
            :toolcall-start
            :toolcall-delta
            :toolcall-delta
            :toolcall-end]
           (mapv :type events)))
    (is (= "Hello world"
           (get-in finalize-result [:assistant-message :content 0 :text])))
    (is (= "Need tool result."
           (get-in finalize-result [:assistant-message :content 1 :thinking])))
    (is (= "sig_stream"
           (get-in finalize-result [:assistant-message :content 1 :signature])))
    (is (= {:x 42}
           (get-in finalize-result [:assistant-message :content 2 :arguments])))
    (is (= :tool-use
           (get-in finalize-result [:assistant-message :stop-reason])))))

(deftest finalize-calculates-usage-costs
  (let [result (sut/finalize
                (stub-env)
                {:model    priced-anthropic-model
                 :response {:status 200
                            :body   (util/json-write
                                     {:content     [{:type "text" :text "ok"}]
                                      :stop_reason "end_turn"
                                      :usage       {:input_tokens                100
                                                    :output_tokens               50
                                                    :cache_read_input_tokens     10
                                                    :cache_creation_input_tokens 5}})}})]
    (is (= {:input 0.1 :output 0.1 :cache-read 0.03 :cache-write 0.02 :total 0.25}
           (get-in result [:assistant-message :usage :cost])))))

(deftest decode-event-stream-usage-calculates-costs
  (let [env        (stub-env)
        init-state {:model priced-anthropic-model}
        start-out  (sut/decode-event env init-state
                                     (util/json-write {:type    "message_start"
                                                       :message {:usage {:input_tokens 100}}}))
        delta-out  (sut/decode-event env (:state start-out)
                                     (util/json-write {:type  "message_delta"
                                                       :delta {:stop_reason "end_turn"}
                                                       :usage {:input_tokens                100
                                                               :output_tokens               50
                                                               :cache_read_input_tokens     10
                                                               :cache_creation_input_tokens 5}}))]
    (is (= {:input 0.1 :output 0.1 :cache-read 0.03 :cache-write 0.02 :total 0.25}
           (get-in delta-out [:state :assistant-message :usage :cost])))))

(defn- string-from-code-units
  [units]
  #?(:clj (String. (char-array (map char units)))
     :cljs (apply str (map (fn [u] (.fromCharCode js/String u)) units))))

(defn- string-with-unpaired-high
  []
  (str "Hello " (string-from-code-units [0xD83D]) " World"))

(defn- valid-emoji-string
  []
  (str "Hello " (string-from-code-units [0xD83D 0xDE48]) " World"))

(deftest build-request-sanitizes-unpaired-surrogates-in-user-and-system
  (let [env         (stub-env)
        context     {:system-prompt (string-with-unpaired-high)
                     :messages      [{:role :user :content (string-with-unpaired-high) :timestamp 1}]}
        request     (sut/build-request env anthropic-model context {:api-key "x"} false)
        payload     (util/json-read (:body request) {:key-fn keyword})
        sys-text    (get-in payload [:system 0 :text])
        usr-content (get-in payload [:messages 0 :content])]
    (is (not (str/includes? (str sys-text) (string-from-code-units [0xD83D])))
        "system prompt should not contain unpaired high surrogate")
    (is (not (str/includes? (str usr-content) (string-from-code-units [0xD83D])))
        "user text should not contain unpaired high surrogate")))

(deftest build-request-preserves-valid-emoji-surrogate-pairs
  (let [env         (stub-env)
        emoji       (valid-emoji-string)
        context     {:messages [{:role :user :content emoji :timestamp 1}]}
        request     (sut/build-request env anthropic-model context {:api-key "x"} false)
        payload     (util/json-read (:body request) {:key-fn keyword})
        usr-content (get-in payload [:messages 0 :content])]
    (is (str/includes? (str usr-content) (valid-emoji-string))
        "valid emoji surrogate pair should be preserved")))
