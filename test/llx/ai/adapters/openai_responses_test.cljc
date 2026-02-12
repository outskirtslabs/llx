(ns llx.ai.adapters.openai-responses-test
  (:require
      [clojure.string :as str]
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [deftest is testing]]])
   [llx.ai.test-util :as util]
   [llx.ai.impl.adapters.openai-responses :as sut]
   [llx.ai.live.models :as live-models]
   [llx.ai.impl.transform-messages :as transform-messages]
   [llx.ai.impl.utils.unicode :as unicode]))

#?(:clj (set! *warn-on-reflection* true))

(def openai-responses-model
  {:id             "gpt-5-mini"
   :name           "GPT-5 Mini"
   :provider       :openai
   :api            :openai-responses
   :base-url       "https://api.openai.com/v1"
   :context-window 400000
   :max-tokens     128000
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text :image}}})

(defn- fixture
  [name]
  (util/read-edn-file (str "test/llx/ai/fixtures/openai_responses/" name ".edn")))

(defn- stub-env
  ([]
   (stub-env {}))
  ([overrides]
   (merge
    {:http/request             (fn [_] {:status 200 :body "{}"})
     :json/encode              util/json-write
     :json/decode              (fn [s _opts] (util/json-read s {:key-fn keyword}))
     :json/decode-safe         (fn [s _opts]
                                 (util/json-read-safe s {:key-fn keyword}))
     :clock/now-ms             (fn [] 1730000000000)
     :id/new                   (fn [] "id-1")
     :unicode/sanitize-payload unicode/sanitize-payload}
    overrides)))

(deftest build-request-converts-context-to-openai-responses-input
  (let [context          (fixture "request_context")
        request          (sut/build-request
                          (stub-env)
                          openai-responses-model
                          context
                          {:api-key           "openai-key"
                           :max-output-tokens 256
                           :temperature       0.25
                           :session-id        "session-1"
                           :cache-control     :short
                           :reasoning         {:effort :high :summary :detailed}}
                          false)
        payload          (util/json-read (:body request) {:key-fn keyword})
        input            (:input payload)
        user-msg         (first (filter #(= "user" (:role %)) input))
        reasoning-items  (filter #(= "reasoning" (:type %)) input)
        function-calls   (filter #(= "function_call" (:type %)) input)
        function-outputs (filter #(= "function_call_output" (:type %)) input)]
    (is (= "https://api.openai.com/v1/responses" (:url request)))
    (is (= "Bearer openai-key" (get-in request [:headers "Authorization"])))
    (is (= false (:stream payload)))
    (is (= 256 (:max_output_tokens payload)))
    (is (= 0.25 (:temperature payload)))
    (is (= "session-1" (:prompt_cache_key payload)))
    (is (= "developer" (:role (first input))))
    (is (= "input_text" (get-in user-msg [:content 0 :type])))
    (is (= "input_image" (get-in user-msg [:content 1 :type])))
    (is (= 1 (count reasoning-items)))
    (is (= 1 (count function-calls)))
    (is (= "call_abc" (:call_id (first function-calls))))
    (is (= "fc_abc" (:id (first function-calls))))
    (is (= 1 (count function-outputs)))
    (is (= "call_abc" (:call_id (first function-outputs))))
    (is (= {:effort "high" :summary "detailed"}
           (:reasoning payload)))
    (is (= ["reasoning.encrypted_content"]
           (:include payload)))))

(deftest build-request-emits-provider-payload-trove-signal
  (util/with-captured-logs!
   (fn [logs*]
     (let [context {:messages [{:role :user :content "hello" :timestamp 1}]}
           request (sut/build-request (stub-env) openai-responses-model context {:api-key "k"} false)
           payload (util/json-read (:body request) {:key-fn keyword})
           event   (util/first-event logs* :llx.obs/provider-payload)]
       (is (util/submap?
            {:id    :llx.obs/provider-payload
             :level :trace
             :data  {:provider :openai
                     :api      :openai-responses
                     :model-id "gpt-5-mini"
                     :stream?  false
                     :url      "https://api.openai.com/v1/responses"
                     :payload  payload}}
            (util/strip-generated event [:call-id])))))))

(deftest normalize-tool-call-id-normalizes-pipe-ids-and-preserves-pairing
  (let [messages  (fixture "pipe_id_prefilled")
        out       (transform-messages/for-target-model
                   messages
                   openai-responses-model
                   {:clock/now-ms           (constantly 4242)
                    :normalize-tool-call-id sut/normalize-tool-call-id})
        call-id   (get-in out [1 :content 0 :id])
        result-id (get-in out [2 :tool-call-id])]
    (is (string? call-id))
    (is (re-find #"\|" call-id))
    (is (<= (count (first (str/split call-id #"\|" 2))) 64))
    (is (<= (count (second (str/split call-id #"\|" 2))) 64))
    (is (= call-id result-id))))

(deftest normalize-tool-call-id-uses-plain-slice-without-hash-suffix
  (let [prefix   (apply str (repeat 90 "a"))
        long-id1 (str "call_" prefix "111|item_" prefix "111")
        out-1    (sut/normalize-tool-call-id long-id1 openai-responses-model nil)
        [c1 i1]  (str/split out-1 #"\|" 2)
        expect-c (subs (str "call_" prefix "111") 0 64)
        expect-i (str "fc_" (subs (str "item_" prefix "111") 0 61))]
    (is (<= (count c1) 64))
    (is (<= (count i1) 64))
    (is (= expect-c c1))
    (is (= expect-i i1))))

(deftest normalize-tool-call-id-trims-trailing-underscores-and-enforces-fc-prefix
  (let [source "call/abc___|item/xyz___"
        out-id (sut/normalize-tool-call-id source openai-responses-model nil)
        [c i]  (str/split out-id #"\|" 2)]
    (is (= "call_abc" c))
    (is (= "fc_item_xyz" i))))

(deftest normalize-tool-call-id-gates-by-provider-allowlist
  (let [non-openai-model (assoc openai-responses-model :provider :openai-compatible)
        source           "call/abc|item/xyz"]
    (is (= source
           (sut/normalize-tool-call-id source non-openai-model nil)))))

(deftest normalize-tool-call-id-matches-upstream-issue-1022-test-id
  (let [normalized        (sut/normalize-tool-call-id live-models/upstream-failing-tool-call-id openai-responses-model nil)
        [call-id item-id] (str/split normalized #"\|" 2)]
    (is (= "call_pAYbIr76hXIjncD9UE4eGfnS" call-id))
    (is (= 64 (count item-id)))
    (is (str/starts-with? item-id "fc_"))
    (is (not (str/ends-with? item-id "_")))))

(deftest cache-retention-mapping
  (testing "cache-control none omits both prompt cache fields"
    (let [context (fixture "request_context")
          request (sut/build-request
                   (stub-env)
                   openai-responses-model
                   context
                   {:api-key       "openai-key"
                    :session-id    "session-none"
                    :cache-control :none}
                   true)
          payload (util/json-read (:body request) {:key-fn keyword})]
      (is (nil? (:prompt_cache_key payload)))
      (is (nil? (:prompt_cache_retention payload)))))

  (testing "cache-control long sets 24h retention for direct api.openai.com"
    (let [context (fixture "request_context")
          request (sut/build-request
                   (stub-env)
                   openai-responses-model
                   context
                   {:api-key       "openai-key"
                    :session-id    "session-long"
                    :cache-control :long}
                   true)
          payload (util/json-read (:body request) {:key-fn keyword})]
      (is (= "session-long" (:prompt_cache_key payload)))
      (is (= "24h" (:prompt_cache_retention payload)))))

  (testing "cache-control long does not set retention for non-openai base url"
    (let [context     (fixture "request_context")
          proxy-model (assoc openai-responses-model :base-url "https://proxy.example.invalid/v1")
          request     (sut/build-request
                       (stub-env)
                       proxy-model
                       context
                       {:api-key       "openai-key"
                        :session-id    "session-long"
                        :cache-control :long}
                       true)
          payload     (util/json-read (:body request) {:key-fn keyword})]
      (is (= "session-long" (:prompt_cache_key payload)))
      (is (nil? (:prompt_cache_retention payload))))))

(deftest finalize-maps-responses-status-to-canonical-stop-reason
  (doseq [[status expected] [["completed" :stop]
                             ["incomplete" :length]
                             ["failed" :error]
                             ["cancelled" :error]
                             ["in_progress" :stop]
                             ["queued" :stop]]]
    (let [result (sut/finalize
                  (stub-env)
                  {:model    openai-responses-model
                   :response {:status 200
                              :body   (util/json-write
                                       {:status status
                                        :output [{:type    "message"
                                                  :id      "msg_1"
                                                  :content [{:type "output_text" :text "ok"}]}]
                                        :usage  {:input_tokens 12 :output_tokens 4 :total_tokens 16}})}})]
      (is (= expected
             (get-in result [:assistant-message :stop-reason])))))

  (let [tool-result (sut/finalize
                     (stub-env)
                     {:model    openai-responses-model
                      :response {:status 200
                                 :body   (util/json-write
                                          {:status "completed"
                                           :output [{:type      "function_call"
                                                     :id        "fc_1"
                                                     :call_id   "call_1"
                                                     :name      "double_number"
                                                     :arguments "{\"value\":21}"}]
                                           :usage  {:input_tokens 12 :output_tokens 4 :total_tokens 16}})}})]
    (is (= :tool-use
           (get-in tool-result [:assistant-message :stop-reason])))))

(deftest finalize-throws-on-unknown-stop-reason
  (is (thrown-with-msg?
       #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
       #"Unknown OpenAI responses stop reason"
       (sut/finalize
        (stub-env)
        {:model    openai-responses-model
         :response {:status 200
                    :body   (util/json-write
                             {:status "brand_new_reason"
                              :output [{:type    "message"
                                        :id      "msg_1"
                                        :content [{:type "output_text" :text "ok"}]}]
                              :usage  {:input_tokens 1 :output_tokens 1 :total_tokens 2}})}}))))

(deftest finalize-calculates-tier-adjusted-cost-when-usage-present
  (let [model-with-cost (assoc openai-responses-model
                               :cost {:input 2.0 :output 4.0 :cache-read 1.0 :cache-write 8.0})
        result          (sut/finalize
                         (stub-env)
                         {:model    model-with-cost
                          :response {:status 200
                                     :body   (util/json-write
                                              {:status       "completed"
                                               :service_tier "priority"
                                               :output       [{:type    "message"
                                                               :id      "msg_1"
                                                               :content [{:type "output_text" :text "ok"}]}]
                                               :usage        {:input_tokens         1500
                                                              :output_tokens        2000
                                                              :total_tokens         3500
                                                              :input_tokens_details {:cached_tokens 500}}})}})
        cost            (get-in result [:assistant-message :usage :cost])]
    (is (> (:input cost) 0.0))
    (is (> (:output cost) 0.0))
    (is (> (:cache-read cost) 0.0))
    ;; input tokens billed: 1000 @ $2/M => 0.002, priority tier x2 => 0.004
    (is (= 0.004 (:input cost)))
    ;; output tokens billed: 2000 @ $4/M => 0.008, priority tier x2 => 0.016
    (is (= 0.016 (:output cost)))
    ;; cache-read tokens billed: 500 @ $1/M => 0.0005, priority tier x2 => 0.001
    (is (= 0.001 (:cache-read cost)))
    (is (= (+ (:input cost) (:output cost) (:cache-read cost) (:cache-write cost))
           (:total cost)))))

(deftest decode-event-stream-contract
  (let [env                                                                  (stub-env)
        chunks                                                               (fixture "stream_events")
        init-state                                                           {:model openai-responses-model}
        {:keys [state events]}
        (reduce (fn [{:keys [state events]} chunk]
                  (let [{next-state :state next-events :events}
                        (sut/decode-event env state (util/json-write chunk))]
                    {:state  next-state
                     :events (into events next-events)}))
                {:state init-state :events []}
                chunks)
        finalize-result                                                      (sut/finalize env state)]
    (is (= [:thinking-start
            :thinking-delta
            :thinking-delta
            :thinking-end
            :text-start
            :text-delta
            :text-delta
            :text-end
            :toolcall-start
            :toolcall-delta
            :toolcall-end]
           (mapv :type events)))
    (is (= "Need tool\n\n"
           (get-in finalize-result [:assistant-message :content 0 :thinking])))
    (is (= "Hello world"
           (get-in finalize-result [:assistant-message :content 1 :text])))
    (is (= {:value 21}
           (get-in finalize-result [:assistant-message :content 2 :arguments])))
    (is (= :tool-use
           (get-in finalize-result [:assistant-message :stop-reason])))
    (is (= {:input 16 :output 5 :cache-read 4 :cache-write 0 :total-tokens 25}
           (select-keys (get-in finalize-result [:assistant-message :usage])
                        [:input :output :cache-read :cache-write :total-tokens])))))

(deftest decode-event-reasoning-summary-part-done-requires-summary-part
  (let [env        (stub-env)
        init-state {:model openai-responses-model}
        start-out  (sut/decode-event env init-state
                                     (util/json-write {:type "response.output_item.added"
                                                      :item {:type    "reasoning"
                                                             :id      "rs_1"
                                                             :summary []}}))
        done-out   (sut/decode-event env (:state start-out)
                                     (util/json-write {:type "response.reasoning_summary_part.done"}))]
    (is (= [:thinking-start] (mapv :type (:events start-out))))
    (is (= [] (:events done-out)))
    (is (= "" (get-in done-out [:state :assistant-message :content 0 :thinking])))))

(deftest stream-error-normalization-contract
  (let [partial-state {:model             openai-responses-model
                       :assistant-message {:role        :assistant
                                           :content     [{:type :text :text "partial"}]
                                           :api         :openai-responses
                                           :provider    :openai
                                           :model       "gpt-5-mini"
                                           :usage       {:input 0                                                                    :output 0 :cache-read 0 :cache-write 0 :total-tokens 0
                                                         :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                                           :stop-reason :stop
                                           :timestamp   1730000000000}}
        out           (sut/normalize-error
                       (stub-env)
                       (ex-info "stream failed" {:status 500 :error "bad gateway"})
                       partial-state)]
    (is (= :error (:stop-reason out)))
    (is (= [{:type :text :text "partial"}] (:content out)))
    (is (string? (:error-message out)))
    (is (str/includes? (:error-message out) "bad gateway"))))

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

(deftest build-request-sanitizes-unpaired-surrogates-in-user-text
  (let [env         (stub-env)
        context     {:system-prompt (string-with-unpaired-high)
                     :messages      [{:role :user :content (string-with-unpaired-high) :timestamp 1}]}
        request     (sut/build-request env openai-responses-model context {:api-key "x"} false)
        payload     (util/json-read (:body request) {:key-fn keyword})
        input-items (:input payload)
        sys-item    (first input-items)
        usr-item    (second input-items)
        unpaired    (string-from-code-units [0xD83D])]
    (is (not (str/includes? (str (:content sys-item)) unpaired))
        "system prompt should not contain unpaired high surrogate")
    (is (not (str/includes? (str (get-in usr-item [:content 0 :text])) unpaired))
        "user text should not contain unpaired high surrogate")))

(deftest build-request-preserves-valid-emoji-surrogate-pairs
  (let [env      (stub-env)
        emoji    (valid-emoji-string)
        context  {:messages [{:role :user :content emoji :timestamp 1}]}
        request  (sut/build-request env openai-responses-model context {:api-key "x"} false)
        payload  (util/json-read (:body request) {:key-fn keyword})
        usr-item (first (:input payload))
        pair     (string-from-code-units [0xD83D 0xDE48])]
    (is (str/includes? (str (get-in usr-item [:content 0 :text])) pair)
        "valid emoji surrogate pair should be preserved")))
