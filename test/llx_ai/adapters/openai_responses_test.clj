(ns llx-ai.adapters.openai-responses-test
  (:require
   [babashka.json :as json]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [llx-ai.adapters.openai-responses :as sut]
   [llx-ai.transform-messages :as transform-messages]))

(set! *warn-on-reflection* true)

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
  (-> (str "test/llx_ai/fixtures/openai_responses/" name ".edn")
      slurp
      edn/read-string))

(defn- stub-env
  ([]
   (stub-env {}))
  ([overrides]
   (merge
    {:json/encode      json/write-str
     :json/decode      (fn [s _opts] (json/read-str s {:key-fn keyword}))
     :json/decode-safe (fn [s _opts]
                         (try
                           (json/read-str s {:key-fn keyword})
                           (catch Exception _ nil)))
     :clock/now-ms     (fn [] 1730000000000)}
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
                           :reasoning         {:level :high :effort :high :summary :detailed}}
                          false)
        payload          (json/read-str (:body request) {:key-fn keyword})
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

(deftest normalize-tool-call-id-avoids-collisions-for-long-prefix-matches
  (let [prefix   (apply str (repeat 90 "a"))
        long-id1 (str "call_" prefix "111|item_" prefix "111")
        long-id2 (str "call_" prefix "222|item_" prefix "222")
        out-1    (sut/normalize-tool-call-id long-id1 openai-responses-model nil)
        out-2    (sut/normalize-tool-call-id long-id2 openai-responses-model nil)
        [c1 i1]  (str/split out-1 #"\|" 2)
        [c2 i2]  (str/split out-2 #"\|" 2)]
    (is (not= out-1 out-2))
    (is (<= (count c1) 64))
    (is (<= (count i1) 64))
    (is (<= (count c2) 64))
    (is (<= (count i2) 64))))

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
          payload (json/read-str (:body request) {:key-fn keyword})]
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
          payload (json/read-str (:body request) {:key-fn keyword})]
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
          payload     (json/read-str (:body request) {:key-fn keyword})]
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
                              :body   (json/write-str
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
                                 :body   (json/write-str
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
       clojure.lang.ExceptionInfo
       #"Unknown OpenAI responses stop reason"
       (sut/finalize
        (stub-env)
        {:model    openai-responses-model
         :response {:status 200
                    :body   (json/write-str
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
                                     :body   (json/write-str
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
                        (sut/decode-event env state (json/write-str chunk))]
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
    (is (.contains ^String (:error-message out) "bad gateway"))))
