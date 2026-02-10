(ns llx-ai.live.provider-parity-smoke-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.jvm :as client]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.live.env :as live-env])
  (:import
   [java.util Base64]
   [java.io File]))

(set! *warn-on-reflection* true)

(def tool-spec
  {:name         "math_operation"
   :description  "Perform basic arithmetic operations"
   :input-schema [:map {:closed true}
                  [:a :int]
                  [:b :int]
                  [:operation [:enum "add" "subtract" "multiply" "divide"]]]})

(def ^:private test-image-base64
  (let [f (File. "test/llx_ai/fixtures/test-image.png")]
    (.encodeToString (Base64/getEncoder) (-> f .toPath java.nio.file.Files/readAllBytes))))

(defn- extract-text
  "Join all :text blocks from an assistant message's :content."
  [assistant-message]
  (->> (:content assistant-message)
       (filter #(= :text (:type %)))
       (map :text)
       (str/join "")))

(defn- execute-math
  "Evaluate a math_operation tool call, returning the numeric result."
  [{:keys [a b operation]}]
  (case operation
    "add"      (+ a b)
    "subtract" (- a b)
    "multiply" (* a b)
    "divide"   (/ a b)
    0))

(defn- basic-text-generation!
  "Two-turn complete conversation. Asserts role, content, usage, error, and
   expected text in each response."
  [model opts]
  (let [context {:system-prompt "You are a helpful assistant. Be concise."
                 :messages      [{:role      :user
                                  :content   "Reply with exactly: 'Hello test successful'"
                                  :timestamp (System/currentTimeMillis)}]}
        first-r (client/complete model context opts)]
    (is (= :assistant (:role first-r)))
    (is (seq (:content first-r)))
    (is (> (+ (get-in first-r [:usage :input] 0)
              (get-in first-r [:usage :cache-read] 0))
           0))
    (is (> (get-in first-r [:usage :output] 0) 0))
    (is (nil? (:error-message first-r)))
    (is (str/includes? (extract-text first-r) "Hello test successful"))

    (let [context2 {:system-prompt "You are a helpful assistant. Be concise."
                    :messages      [(first (:messages context))
                                    first-r
                                    {:role      :user
                                     :content   "Now say 'Goodbye test successful'"
                                     :timestamp (System/currentTimeMillis)}]}
          second-r (client/complete model context2 opts)]
      (is (= :assistant (:role second-r)))
      (is (seq (:content second-r)))
      (is (> (+ (get-in second-r [:usage :input] 0)
                (get-in second-r [:usage :cache-read] 0))
             0))
      (is (> (get-in second-r [:usage :output] 0) 0))
      (is (nil? (:error-message second-r)))
      (is (str/includes? (extract-text second-r) "Goodbye test successful")))))

(defn- handle-tool-call!
  "Stream with calculator tool. Walk events asserting toolcall-start/-delta/-end
   shapes, argument parsing, and final result stop-reason."
  [model opts]
  (let [stream    (client/stream model
                                 {:system-prompt "You are a helpful assistant that uses tools when asked."
                                  :messages      [{:role      :user
                                                   :content   "Calculate 15 + 27 using the math_operation tool."
                                                   :timestamp (System/currentTimeMillis)}]
                                  :tools         [tool-spec]}
                                 opts)
        events    (event-stream/drain! stream)
        result    (event-stream/result stream)
        saw-start (atom false)
        saw-delta (atom false)
        saw-end   (atom false)]
    (doseq [evt events]
      (case (:type evt)
        :toolcall-start
        (do (reset! saw-start true)
            (is (= "math_operation" (:name evt)))
            (is (seq (:id evt))))

        :toolcall-delta
        (do (reset! saw-delta true)
            (is (= "math_operation" (:name evt)))
            (is (some? (:arguments evt)))
            (is (map? (:arguments evt))))

        :toolcall-end
        (do (reset! saw-end true)
            (let [args (:arguments evt)]
              (is (= 15 (:a args)))
              (is (= 27 (:b args)))
              (is (contains? #{"add" "subtract" "multiply" "divide"} (:operation args)))))

        nil))

    (is @saw-start "expected :toolcall-start event")
    (is @saw-delta "expected :toolcall-delta event")
    (is @saw-end "expected :toolcall-end event")
    (is (= :tool-use (:stop-reason result)))
    (let [tc-block (some #(when (= :tool-call (:type %)) %) (:content result))]
      (is (some? tc-block) "result should contain a :tool-call content block")
      (is (= "math_operation" (:name tc-block)))
      (is (seq (:id tc-block))))))

(defn- handle-streaming!
  "Stream 'count from 1 to 3'. Assert text-start/-delta/-end all seen,
   accumulated text length > 0, and result has :text content block."
  [model opts]
  (let [stream     (client/stream model
                                  {:system-prompt "You are a helpful assistant."
                                   :messages      [{:role      :user
                                                    :content   "Count from 1 to 3"
                                                    :timestamp (System/currentTimeMillis)}]}
                                  opts)
        events     (event-stream/drain! stream)
        result     (event-stream/result stream)
        types      (set (map :type events))
        text-accum (apply str (keep :text (filter #(= :text-delta (:type %)) events)))]
    (is (contains? types :text-start) "expected :text-start event")
    (is (contains? types :text-delta) "expected :text-delta event")
    (is (contains? types :text-end) "expected :text-end event")
    (is (pos? (count text-accum)) "accumulated text should be non-empty")
    (is (some #(= :text (:type %)) (:content result))
        "result should contain a :text content block")))

(defn- handle-thinking!
  "Stream with randomized math prompt. Assert thinking-start/-delta/-end all
   seen, accumulated thinking length > 0, stop-reason is :stop, and result has
   :thinking content block."
  [model opts]
  (let [rand-num       (rand-int 256)
        stream         (client/stream model
                                      {:system-prompt "You are a helpful assistant."
                                       :messages      [{:role      :user
                                                        :content   (str "Think long and hard about " rand-num " + 27. Think step by step. Then output the result.")
                                                        :timestamp (System/currentTimeMillis)}]}
                                      opts)
        events         (event-stream/drain! stream)
        result         (event-stream/result stream)
        types          (set (map :type events))
        thinking-accum (apply str (keep :thinking (filter #(= :thinking-delta (:type %)) events)))]
    (is (= :stop (:stop-reason result))
        (str "expected :stop but got " (:stop-reason result) " error: " (:error-message result)))
    (is (contains? types :thinking-start) "expected :thinking-start event")
    (is (contains? types :thinking-delta) "expected :thinking-delta event")
    (is (contains? types :thinking-end) "expected :thinking-end event")
    (is (pos? (count thinking-accum)) "accumulated thinking should be non-empty")
    (is (some #(= :thinking (:type %)) (:content result))
        "result should contain a :thinking content block")))

(defn- handle-image!
  "Complete with text + image content (test-image.png). Skips if model doesn't
   support image input. Asserts response text contains 'green' and 'triangle'."
  [model opts]
  (if-not (contains? (get-in model [:capabilities :input]) :image)
    (is true (str "Skipping image test - model " (:id model) " doesn't support images"))
    (let [result (client/complete model
                                  {:system-prompt "You are a helpful assistant."
                                   :messages      [{:role      :user
                                                    :content   [{:type :text
                                                                 :text "What do you see in this image? Please describe the shape (circle, rectangle, square, triangle, ...) and color (red, blue, green, ...). You MUST reply in English."}
                                                                {:type      :image
                                                                 :data      test-image-base64
                                                                 :mime-type "image/png"}]
                                                    :timestamp (System/currentTimeMillis)}]}
                                  opts)
          text   (str/lower-case (extract-text result))]
      (is (pos? (count (:content result))))
      (is (str/includes? text "green") "response should mention 'green'")
      (is (str/includes? text "triangle") "response should mention 'triangle'"))))

(defn- multi-turn!
  "Complete loop (max 5 turns). Prompt asks to calculate 42 * 17 and 453 + 434.
   Execute tool calls, send results back, loop until :stop or max turns.
   Assert we saw thinking or tool calls, and text contains '714' and '887'."
  [model opts]
  (let [initial-msg   {:role      :user
                       :content   "Think about this briefly, then calculate 42 * 17 and 453 + 434 using the math_operation tool."
                       :timestamp (System/currentTimeMillis)}
        max-turns     5
        all-text      (atom "")
        seen-thinking (atom false)
        seen-tools    (atom false)]
    (loop [messages [initial-msg]
           turn     0]
      (when (< turn max-turns)
        (let [response     (client/complete model
                                            {:system-prompt "You are a helpful assistant that can use tools to answer questions."
                                             :messages      messages
                                             :tools         [tool-spec]}
                                            opts)
              messages     (conj messages response)
              tool-results (reduce
                            (fn [results block]
                              (case (:type block)
                                :text
                                (do (swap! all-text str (:text block))
                                    results)

                                :thinking
                                (do (reset! seen-thinking true)
                                    results)

                                :tool-call
                                (do (reset! seen-tools true)
                                    (is (= "math_operation" (:name block)))
                                    (is (seq (:id block)))
                                    (is (some? (:arguments block)))
                                    (let [math-result (execute-math (:arguments block))]
                                      (conj results
                                            {:role         :tool-result
                                             :tool-call-id (:id block)
                                             :tool-name    (:name block)
                                             :content      [{:type :text :text (str math-result)}]
                                             :is-error?    false
                                             :timestamp    (System/currentTimeMillis)})))

                                ;; ignore other block types
                                results))
                            []
                            (:content response))]
          (is (not= :error (:stop-reason response))
              (str "unexpected error: " (:error-message response)))
          (if (= :stop (:stop-reason response))
            nil ;; done
            (recur (into messages tool-results)
                   (inc turn))))))

    (is (or @seen-thinking @seen-tools)
        "should have seen thinking or tool calls")
    (is (seq @all-text) "should have accumulated some text")
    (is (str/includes? @all-text "714")
        "accumulated text should contain 714 (42 * 17)")
    (is (str/includes? @all-text "887")
        "accumulated text should contain 887 (453 + 434)")))

(def openai-responses-model
  {:id             (or (live-env/get-env "LLX_LIVE_OPENAI_RESPONSES_MODEL") "gpt-5-mini")
   :name           "OpenAI Responses parity model"
   :provider       :openai
   :api            :openai-responses
   :base-url       (or (live-env/get-env "LLX_LIVE_OPENAI_RESPONSES_BASE_URL") "https://api.openai.com/v1")
   :context-window 400000
   :max-tokens     128000
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text :image}}})

(def openai-completions-model
  {:id             (or (live-env/get-env "LLX_LIVE_OPENAI_MODEL") "gpt-4o-mini")
   :name           "OpenAI Completions parity model"
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text :image}}})

(def anthropic-model
  {:id             (or (live-env/get-env "LLX_LIVE_ANTHROPIC_MODEL") "claude-3-5-haiku-20241022")
   :name           "Anthropic parity model"
   :provider       :anthropic
   :api            :anthropic-messages
   :base-url       (or (live-env/get-env "LLX_LIVE_ANTHROPIC_BASE_URL") "https://api.anthropic.com")
   :context-window 200000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text :image}}})

(def google-model
  {:id             (or (live-env/get-env "LLX_LIVE_GOOGLE_MODEL") "gemini-2.5-flash")
   :name           "Google parity model"
   :provider       :google
   :api            :google-generative-ai
   :base-url       (or (live-env/get-env "LLX_LIVE_GOOGLE_BASE_URL") "https://generativelanguage.googleapis.com/v1beta")
   :context-window 1048576
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text :image}}})

(def mistral-model
  {:id             (or (live-env/get-env "LLX_LIVE_MISTRAL_MODEL") "devstral-medium-latest")
   :name           "Mistral parity model"
   :provider       :mistral
   :api            :openai-completions
   :base-url       (or (live-env/get-env "LLX_LIVE_MISTRAL_BASE_URL") "https://api.mistral.ai/v1")
   :context-window 128000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(def mistral-pixtral-model
  {:id             (or (live-env/get-env "LLX_LIVE_MISTRAL_PIXTRAL_MODEL") "pixtral-12b")
   :name           "Mistral Pixtral parity model"
   :provider       :mistral
   :api            :openai-completions
   :base-url       (or (live-env/get-env "LLX_LIVE_MISTRAL_BASE_URL") "https://api.mistral.ai/v1")
   :context-window 128000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text :image}}})

(def ollama-model
  {:id             (or (live-env/get-env "LLX_LIVE_OLLAMA_MODEL") "gpt-oss:20b")
   :name           "Ollama GPT-OSS 20B"
   :provider       :openai-compatible
   :api            :openai-completions
   :base-url       (or (live-env/get-env "LLX_LIVE_OLLAMA_BASE_URL") "http://localhost:11434/v1")
   :context-window 128000
   :max-tokens     16000
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text}}})

(deftest live-parity-google
  (let [api-key (live-env/get-env "GEMINI_API_KEY")
        opts    {:api-key api-key :max-output-tokens 256}]
    (testing "basic-text-generation"
      (basic-text-generation! google-model opts))
    (testing "handle-tool-call"
      (handle-tool-call! google-model opts))
    (testing "handle-streaming"
      (handle-streaming! google-model opts))
    (testing "handle-thinking"
      (handle-thinking! google-model (assoc opts
                                            :max-output-tokens 2048
                                            :reasoning {:level :high})))
    (testing "handle-image"
      (handle-image! google-model opts))
    (testing "multi-turn"
      (multi-turn! google-model (assoc opts
                                       :max-output-tokens 2048
                                       :reasoning {:level :high})))))

(deftest live-parity-openai-completions
  (let [api-key (live-env/get-env "OPENAI_API_KEY")
        opts    {:api-key api-key :max-output-tokens 256}]
    (testing "basic-text-generation"
      (basic-text-generation! openai-completions-model opts))
    (testing "handle-tool-call"
      (handle-tool-call! openai-completions-model opts))
    (testing "handle-streaming"
      (handle-streaming! openai-completions-model opts))
    (testing "handle-image"
      (handle-image! openai-completions-model opts))))

(deftest live-parity-openai-responses
  (let [api-key (live-env/get-env "OPENAI_API_KEY")
        opts    {:api-key api-key :max-output-tokens 256}]
    (testing "basic-text-generation"
      (basic-text-generation! openai-responses-model opts))
    (testing "handle-tool-call"
      (handle-tool-call! openai-responses-model opts))
    (testing "handle-streaming"
      (handle-streaming! openai-responses-model opts))
    (testing "handle-thinking"
      (handle-thinking! openai-responses-model (assoc opts
                                                      :max-output-tokens 2048
                                                      :reasoning {:level :high :effort :high})))
    (testing "handle-image"
      (handle-image! openai-responses-model opts))
    (testing "multi-turn"
      (multi-turn! openai-responses-model (assoc opts
                                                 :max-output-tokens 2048
                                                 :reasoning {:level :high :effort :high})))))

(deftest live-parity-anthropic
  (let [api-key (live-env/get-env "ANTHROPIC_API_KEY")
        opts    {:api-key api-key :max-output-tokens 256}]
    (testing "basic-text-generation"
      (basic-text-generation! anthropic-model (assoc opts :reasoning {:level :high})))
    (testing "handle-tool-call"
      (handle-tool-call! anthropic-model opts))
    (testing "handle-streaming"
      (handle-streaming! anthropic-model opts))
    (testing "handle-image"
      (handle-image! anthropic-model opts))))

(deftest live-parity-mistral-devstral
  (let [api-key (live-env/get-env "MISTRAL_API_KEY")
        opts    {:api-key api-key :max-output-tokens 256}]
    (testing "basic-text-generation"
      (basic-text-generation! mistral-model opts))
    (testing "handle-tool-call"
      (handle-tool-call! mistral-model opts))
    (testing "handle-streaming"
      (handle-streaming! mistral-model opts))
    ;; FIXME: thinking skipped -- TS test has empty body due to 422 error
    (testing "multi-turn"
      (multi-turn! mistral-model (assoc opts
                                        :max-output-tokens 2048
                                        :reasoning {:level :medium})))))

(deftest live-parity-mistral-pixtral
  (let [api-key (live-env/get-env "MISTRAL_API_KEY")
        opts    {:api-key api-key :max-output-tokens 256}]
    (testing "basic-text-generation"
      (basic-text-generation! mistral-pixtral-model opts))
    (testing "handle-tool-call"
      (handle-tool-call! mistral-pixtral-model opts))
    (testing "handle-streaming"
      (handle-streaming! mistral-pixtral-model opts))
    (testing "handle-image"
      (handle-image! mistral-pixtral-model opts))))

(deftest live-parity-ollama
  (let [opts {:max-output-tokens 256}]
    (testing "basic-text-generation"
      (basic-text-generation! ollama-model opts))
    (testing "handle-tool-call"
      (handle-tool-call! ollama-model opts))
    (testing "handle-streaming"
      (handle-streaming! ollama-model opts))
    (testing "handle-thinking"
      (handle-thinking! ollama-model (assoc opts
                                            :max-output-tokens 2048
                                            :reasoning {:level :medium})))
    (testing "multi-turn"
      (multi-turn! ollama-model (assoc opts
                                       :max-output-tokens 2048
                                       :reasoning {:level :medium})))))
