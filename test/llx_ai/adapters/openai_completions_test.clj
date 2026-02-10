(ns llx-ai.adapters.openai-completions-test
  (:require
   [babashka.json :as json]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [llx-ai.adapters.openai-completions :as sut]
   [llx-ai.utils.unicode :as unicode]))

(set! *warn-on-reflection* true)

(def openai-model
  {:id             "gpt-4o-mini"
   :name           "GPT-4o Mini"
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text :image}}})

(def mistral-model
  {:id             "devstral-medium-latest"
   :name           "Devstral Medium"
   :provider       :mistral
   :api            :openai-completions
   :base-url       "https://api.mistral.ai/v1"
   :context-window 128000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text :image}}})

(def openai-compatible-model
  {:id             "devstral-small-2:latest"
   :name           "Ollama Devstral"
   :provider       :openai-compatible
   :api            :openai-completions
   :base-url       "http://localhost:11434/v1"
   :context-window 32768
   :max-tokens     4096
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(def priced-openai-model
  (assoc openai-model :cost {:input 1000.0 :output 2000.0 :cache-read 3000.0 :cache-write 4000.0}))

(defn- fixture
  [name]
  (-> (str "test/llx_ai/fixtures/openai_completions/" name ".edn")
      slurp
      edn/read-string))

(defn- stub-env
  ([]
   (stub-env {}))
  ([overrides]
   (merge
    {:http/request             (fn [_] {:status 200 :body "{}"})
     :json/encode              json/write-str
     :json/decode              (fn [s _opts] (json/read-str s {:key-fn keyword}))
     :json/decode-safe         (fn [s _opts]
                                 (try
                                   (json/read-str s {:key-fn keyword})
                                   (catch Exception _ nil)))
     :clock/now-ms             (fn [] 1730000000000)
     :id/new                   (fn [] "generated-id")
     :unicode/sanitize-payload unicode/sanitize-payload}
    overrides)))

(deftest build-request-forwards-tools-and-tool-choice-with-compat-token-field
  (let [model   (assoc openai-model
                       :compat {:token-field            :max_tokens
                                :supports-strict-tools? false})
        context {:messages [{:role :user :content "call ping" :timestamp 1}]
                 :tools    [{:name         "ping"
                             :description  "Ping tool"
                             :input-schema [:map [:ok :boolean]]}]}
        request (sut/build-request (stub-env) model context {:api-key "k" :max-output-tokens 256 :tool-choice "ping"} false)
        payload (json/read-str (:body request) {:key-fn keyword})]
    (is (= 256 (:max_tokens payload)))
    (is (nil? (:max_completion_tokens payload)))
    (is (= {:type "function" :function {:name "ping"}}
           (:tool_choice payload)))
    (is (= "ping" (get-in payload [:tools 0 :function :name])))
    (is (= false (contains? (get-in payload [:tools 0 :function]) :strict)))))

(deftest build-request-tool-choice-sentinel-strings-pass-through
  (let [base-context {:messages [{:role :user :content "call ping" :timestamp 1}]}
        mk-payload   (fn [tool-choice]
                       (-> (sut/build-request (stub-env) openai-model base-context
                                              {:api-key "k" :tool-choice tool-choice}
                                              false)
                           :body
                           (json/read-str {:key-fn keyword})))]
    (is (= "auto" (:tool_choice (mk-payload "auto"))))
    (is (= "none" (:tool_choice (mk-payload "none"))))
    (is (= "required" (:tool_choice (mk-payload "required"))))))

(deftest build-request-batches-tool-result-images-for-openai-completions
  (let [context   (fixture "mistral_request_context")
        request   (sut/build-request (stub-env) mistral-model context {:api-key "mistral-key"} false)
        payload   (json/read-str (:body request) {:key-fn keyword})
        messages  (:messages payload)
        roles     (mapv :role messages)
        tool-1    (nth messages 2)
        tool-2    (nth messages 3)
        image-msg (nth messages 4)
        user-msg  (nth messages 5)]
    (is (= ["user" "assistant" "tool" "tool" "user" "user"] roles))
    (is (= "vision" (:name tool-1)))
    (is (= "vision" (:name tool-2)))
    (is (= "(see attached image)" (:content tool-1)))
    (is (= "secondary" (:content tool-2)))
    (is (= "Attached image(s) from tool result:" (get-in image-msg [:content 0 :text])))
    (is (= 2 (count (filter #(= "image_url" (:type %)) (:content image-msg)))))
    (is (= "continue" (:content user-msg)))))

(deftest decode-event-stream-contract
  (let [env       (stub-env)
        chunks    (fixture "mistral_stream_events")
        reduced   (reduce (fn [{:keys [state events]} chunk]
                            (let [{next-state :state next-events :events}
                                  (sut/decode-event env state (json/write-str chunk))]
                              {:state  next-state
                               :events (into events next-events)}))
                          {:state  {:model mistral-model}
                           :events []}
                          chunks)
        finalized (sut/finalize env (:state reduced))]
    (is (= [:toolcall-start
            :toolcall-delta
            :toolcall-delta
            :toolcall-end
            :text-start
            :text-delta]
           (mapv :type (:events reduced))))
    (is (= {:path "img-1.png"}
           (get-in finalized [:assistant-message :content 0 :arguments])))
    (is (= "done"
           (get-in finalized [:assistant-message :content 1 :text])))
    (is (= :stop
           (get-in finalized [:assistant-message :stop-reason])))))

(deftest normalize-tool-call-id-mistral-shape
  (testing "normalization is deterministic 9-char alphanumeric"
    (let [source   "call_with+symbols/and spaces"
          first-id (sut/normalize-tool-call-id source mistral-model nil)
          next-id  (sut/normalize-tool-call-id source mistral-model nil)]
      (is (= first-id next-id))
      (is (= 9 (count first-id)))
      (is (re-matches #"[A-Za-z0-9]{9}" first-id)))))

(deftest normalize-tool-call-id-mistral-avoids-collision-for-distinct-source-ids
  (let [assistant-message {:role        :assistant
                           :content     [{:type      :tool-call
                                          :id        "call_abcdefgh1--with-extra"
                                          :name      "one"
                                          :arguments {}}
                                         {:type      :tool-call
                                          :id        "call_abcdefgh2--with-extra"
                                          :name      "two"
                                          :arguments {}}]
                           :api         :openai-responses
                           :provider    :openai
                           :model       "gpt-5-mini"
                           :usage       {:input 0                                                                    :output 0 :cache-read 0 :cache-write 0 :total-tokens 0
                                         :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                           :stop-reason :tool-use
                           :timestamp   1}
        id-1              (sut/normalize-tool-call-id "call_abcdefgh1--with-extra" mistral-model assistant-message)
        id-2              (sut/normalize-tool-call-id "call_abcdefgh2--with-extra" mistral-model assistant-message)]
    (is (= 9 (count id-1)))
    (is (= 9 (count id-2)))
    (is (re-matches #"[A-Za-z0-9]{9}" id-1))
    (is (re-matches #"[A-Za-z0-9]{9}" id-2))
    (is (not= id-1 id-2))))

(deftest build-request-openai-compatible-omits-auth-and-respects-compat-overrides
  (let [env     (stub-env)
        model   (assoc openai-compatible-model
                       :compat {:store?                 false
                                :supports-usage-stream? false
                                :supports-strict-tools? false})
        req     (sut/build-request env model
                                   {:messages [{:role :user :content "ping" :timestamp 1}]
                                    :tools    [{:name         "ping"
                                                :description  "Ping tool"
                                                :input-schema [:map [:ok :boolean]]}]}
                                   {:max-output-tokens 128}
                                   true)
        body    (json/read-str (:body req) {:key-fn keyword})
        tool-fn (get-in body [:tools 0 :function])]
    (is (= nil (get-in req [:headers "Authorization"])))
    (is (nil? (:store body)))
    (is (nil? (:stream_options body)))
    (is (= 128 (:max_completion_tokens body)))
    (is (= "ping" (:name tool-fn)))
    (is (= false (contains? tool-fn :strict)))))

(deftest build-request-reads-provider-specific-env-api-key
  (let [env-openai  (stub-env {:env/get (fn [k]
                                          (case k
                                            "OPENAI_API_KEY" "openai-env-key"
                                            nil))})
        req-openai  (sut/build-request env-openai openai-model
                                       {:messages [{:role :user :content "ping" :timestamp 1}]}
                                       {}
                                       false)
        env-mistral (stub-env {:env/get (fn [k]
                                          (case k
                                            "MISTRAL_API_KEY" "mistral-env-key"
                                            nil))})
        req-mistral (sut/build-request env-mistral mistral-model
                                       {:messages [{:role :user :content "ping" :timestamp 1}]}
                                       {}
                                       false)]
    (is (= "Bearer openai-env-key" (get-in req-openai [:headers "Authorization"])))
    (is (= "Bearer mistral-env-key" (get-in req-mistral [:headers "Authorization"])))))

(deftest build-request-missing-api-key-message-includes-env-var-name
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Set MISTRAL_API_KEY or pass :api-key"
       (sut/build-request (stub-env) mistral-model
                          {:messages [{:role :user :content "ping" :timestamp 1}]}
                          {}
                          false))))

(def reasoning-model
  {:id             "qwq-32b"
   :name           "QWQ 32B"
   :provider       :openai-compatible
   :api            :openai-completions
   :base-url       "http://localhost:11434/v1"
   :context-window 32768
   :max-tokens     4096
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(deftest build-request-includes-reasoning-effort-for-reasoning-model
  (let [context {:messages [{:role :user :content "think about this" :timestamp 1}]}
        request (sut/build-request (stub-env) reasoning-model context
                                   {:reasoning {:level :medium}}
                                   false)
        payload (json/read-str (:body request) {:key-fn keyword})]
    (is (= "medium" (:reasoning_effort payload)))))

(deftest build-request-omits-reasoning-effort-when-model-lacks-reasoning
  (let [context {:messages [{:role :user :content "think about this" :timestamp 1}]}
        request (sut/build-request (stub-env) openai-compatible-model context
                                   {:reasoning {:level :medium}}
                                   false)
        payload (json/read-str (:body request) {:key-fn keyword})]
    (is (nil? (:reasoning_effort payload)))))

(deftest decode-event-emits-thinking-events-for-reasoning-content
  (let [env                    (stub-env)
        state                  (sut/init-stream-state env reasoning-model)
        chunk                  {:choices [{:delta {:reasoning_content "thinking..."}}]}
        {:keys [state events]} (sut/decode-event env state (json/write-str chunk))]
    (is (= [:thinking-start :thinking-delta] (mapv :type events)))
    (is (= "thinking..." (:thinking (second events))))
    (is (= "thinking..." (get-in state [:assistant-message :content 0 :thinking])))))

(deftest response->assistant-message-calculates-usage-costs
  (let [response {:status 200
                  :body   (json/write-str
                           {:choices [{:finish_reason "stop"
                                       :message       {:role "assistant" :content "ok"}}]
                            :usage   {:prompt_tokens             120
                                      :prompt_tokens_details     {:cached_tokens 20}
                                      :completion_tokens         30
                                      :completion_tokens_details {:reasoning_tokens 10}
                                      :total_tokens              160}})}
        out      (sut/response->assistant-message (stub-env) priced-openai-model response)]
    (is (= {:input 0.1 :output 0.08 :cache-read 0.06 :cache-write 0.0 :total 0.24}
           (get-in out [:usage :cost])))))

(deftest decode-event-stream-usage-calculates-costs
  (let [state (sut/init-stream-state (stub-env) priced-openai-model)
        chunk {:choices [{:delta {:content "ok"}}]
               :usage   {:prompt_tokens             90
                         :prompt_tokens_details     {:cached_tokens 30}
                         :completion_tokens         20
                         :completion_tokens_details {:reasoning_tokens 10}
                         :total_tokens              120}}
        out   (sut/decode-event (stub-env) state (json/write-str chunk))]
    (is (= {:input 0.06 :output 0.06 :cache-read 0.09 :cache-write 0.0 :total 0.21}
           (get-in out [:state :assistant-message :usage :cost])))))

(deftest decode-event-tracks-thinking-signature
  (let [state           (sut/init-stream-state (stub-env) reasoning-model)
        chunk           {:choices [{:delta {:reasoning_content "thinking..."}}]}
        {:keys [state]} (sut/decode-event (stub-env) state (json/write-str chunk))]
    (is (= {:type :thinking :thinking "thinking..." :signature "reasoning_content"}
           (get-in state [:assistant-message :content 0])))))

(deftest decode-event-extracts-reasoning-details-to-tool-call-signature
  (let [state           (sut/init-stream-state (stub-env) reasoning-model)
        chunk           {:choices [{:delta {:tool_calls        [{:index    0
                                                                 :id       "detail_id_1"
                                                                 :type     "function"
                                                                 :function {:name      "search"
                                                                            :arguments "{\"q\":\"foo\"}"}}]
                                            :reasoning_details [{:type "reasoning.encrypted"
                                                                 :id   "detail_id_1"
                                                                 :data "encrypted-data"}]}}]}
        {:keys [state]} (sut/decode-event (stub-env) state (json/write-str chunk))
        tool-block      (first (filter #(= :tool-call (:type %))
                                       (get-in state [:assistant-message :content])))
        parsed          (when (:signature tool-block)
                          (json/read-str (:signature tool-block) {:key-fn keyword}))]
    (is (= {:type "reasoning.encrypted" :id "detail_id_1" :data "encrypted-data"}
           parsed))))

(deftest convert-message-restores-thinking-field-from-signature
  (let [message {:role        :assistant
                 :content     [{:type :thinking :thinking "deep thought" :signature "reasoning_content"}
                               {:type :text :text "answer"}]
                 :api         :openai-completions
                 :provider    :openai
                 :model       "gpt-4o-mini"
                 :usage       {:input 0                                                                    :output 0 :cache-read 0 :cache-write 0 :total-tokens 0
                               :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                 :stop-reason :stop
                 :timestamp   1}
        model   (assoc openai-model :capabilities {:reasoning? true :input #{:text}})
        result  (#'sut/convert-message (stub-env) model message)]
    (is (= {:role "assistant" :content "answer" :reasoning_content "deep thought"}
           result))))

(deftest convert-message-reconstructs-reasoning-details-from-tool-signatures
  (let [detail  {:type "reasoning.encrypted" :id "tc_1" :data "enc"}
        message {:role        :assistant
                 :content     [{:type      :tool-call              :id "tc_1" :name "search"
                                :arguments {:q "foo"}
                                :signature (json/write-str detail)}]
                 :api         :openai-completions
                 :provider    :openai
                 :model       "gpt-4o-mini"
                 :usage       {:input 0                                                                    :output 0 :cache-read 0 :cache-write 0 :total-tokens 0
                               :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                 :stop-reason :tool-use
                 :timestamp   1}
        result  (#'sut/convert-message (stub-env) openai-model message)]
    (is (= {:role              "assistant"
            :content           ""
            :tool_calls        [{:id       "tc_1"
                                 :type     "function"
                                 :function {:name "search" :arguments "{\"q\":\"foo\"}"}}]
            :reasoning_details [{:type "reasoning.encrypted" :id "tc_1" :data "enc"}]}
           result))))

(deftest build-request-zai-thinking-format-sends-thinking-object
  (let [zai-model (assoc reasoning-model
                         :base-url "https://api.z.ai/v1"
                         :compat {:thinking-format :zai})
        request   (sut/build-request (stub-env) zai-model
                                     {:messages [{:role :user :content "think" :timestamp 1}]}
                                     {:reasoning {:level :medium}} false)
        payload   (json/read-str (:body request) {:key-fn keyword})]
    (is (= {:thinking {:type "enabled"}}
           (select-keys payload [:thinking :reasoning_effort])))))

(deftest build-request-zai-disables-thinking-when-no-reasoning
  (let [zai-model (assoc reasoning-model
                         :base-url "https://api.z.ai/v1"
                         :compat {:thinking-format :zai})
        request   (sut/build-request (stub-env) zai-model
                                     {:messages [{:role :user :content "chat" :timestamp 1}]}
                                     {} false)
        payload   (json/read-str (:body request) {:key-fn keyword})]
    (is (= {:thinking {:type "disabled"}}
           (select-keys payload [:thinking :reasoning_effort])))))

(deftest build-request-qwen-thinking-format-sends-enable-thinking
  (let [qwen-model (assoc reasoning-model
                          :compat {:thinking-format :qwen})
        request    (sut/build-request (stub-env) qwen-model
                                      {:messages [{:role :user :content "think" :timestamp 1}]}
                                      {:reasoning {:level :high}} false)
        payload    (json/read-str (:body request) {:key-fn keyword})]
    (is (= {:enable_thinking true}
           (select-keys payload [:enable_thinking :reasoning_effort])))))

(deftest decode-event-thinking-to-text-transition
  (let [env                             (stub-env)
        state0                          (sut/init-stream-state env reasoning-model)
        chunk1                          {:choices [{:delta {:reasoning_content "reason"}}]}
        chunk2                          {:choices [{:delta {:content "answer"}}]}
        {state1 :state events1 :events} (sut/decode-event env state0 (json/write-str chunk1))
        {state2 :state events2 :events} (sut/decode-event env state1 (json/write-str chunk2))
        finalized                       (sut/finalize env state2)]
    (is (= [:thinking-start :thinking-delta] (mapv :type events1)))
    (is (= [:thinking-end :text-start :text-delta] (mapv :type events2)))
    (is (= "answer" (:text (nth events2 2))))
    (is (= :thinking (get-in finalized [:assistant-message :content 0 :type])))
    (is (= "reason" (get-in finalized [:assistant-message :content 0 :thinking])))
    (is (= :text (get-in finalized [:assistant-message :content 1 :type])))
    (is (= "answer" (get-in finalized [:assistant-message :content 1 :text])))))

(defn- string-with-unpaired-high
  []
  (str "Hello " (String. (char-array [(char 0xD83D)])) " World"))

(defn- string-with-unpaired-low
  []
  (str "Hello " (String. (char-array [(char 0xDE48)])) " World"))

(defn- valid-emoji-string
  []
  (str "Hello " (String. (char-array [(char 0xD83D) (char 0xDE48)])) " World"))

(deftest build-request-sanitizes-unpaired-surrogates-in-user-text
  (let [env     (stub-env)
        context {:system-prompt (string-with-unpaired-high)
                 :messages      [{:role :user :content (string-with-unpaired-low) :timestamp 1}]}
        request (sut/build-request env openai-model context {:api-key "x"} false)
        payload (json/read-str (:body request) {:key-fn keyword})
        sys-msg (first (:messages payload))
        usr-msg (second (:messages payload))]
    (is (not (str/includes? (:content sys-msg) (String. (char-array [(char 0xD83D)]))))
        "system prompt should not contain unpaired high surrogate")
    (is (not (str/includes? (:content usr-msg) (String. (char-array [(char 0xDE48)]))))
        "user text should not contain unpaired low surrogate")))

(deftest build-request-preserves-valid-emoji-surrogate-pairs
  (let [env     (stub-env)
        emoji   (valid-emoji-string)
        context {:messages [{:role :user :content emoji :timestamp 1}]}
        request (sut/build-request env openai-model context {:api-key "x"} false)
        payload (json/read-str (:body request) {:key-fn keyword})
        usr-msg (first (:messages payload))]
    (is (str/includes? (:content usr-msg) (String. (char-array [(char 0xD83D) (char 0xDE48)])))
        "valid emoji surrogate pair should be preserved")))

(deftest build-request-sanitizes-unpaired-surrogates-in-tool-result-text
  (let [env      (stub-env)
        context  {:messages [{:role :user :content "use tool" :timestamp 1}
                             {:role        :assistant
                              :content     [{:type :tool-call :id "call_1" :name "t" :arguments {}}]
                              :api         :openai-completions
                              :provider    :openai
                              :model       "gpt-4o-mini"
                              :usage       {:input 1                                                                    :output 1 :cache-read 0 :cache-write 0 :total-tokens 2
                                            :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                              :stop-reason :tool-use
                              :timestamp   2}
                             {:role         :tool-result
                              :tool-call-id "call_1"
                              :tool-name    "t"
                              :content      [{:type :text :text (string-with-unpaired-high)}]
                              :is-error?    false
                              :timestamp    3}]}
        request  (sut/build-request env openai-model context {:api-key "x"} false)
        payload  (json/read-str (:body request) {:key-fn keyword})
        tool-msg (nth (:messages payload) 2)]
    (is (not (str/includes? (:content tool-msg) (String. (char-array [(char 0xD83D)]))))
        "tool result text should not contain unpaired high surrogate")))
