(ns ^{:kaocha/parallelize? true} llx.ai.live.provider-parity-smoke-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer [deftest is testing]]])
   #?@(:clj [[llx.ai.test-util :as util]]
       :cljs [[llx.ai.test-util :as util :include-macros true]])
   [clojure.string :as str]
   [llx.ai :as client]
   [llx.ai.live.models :as models]
   [promesa.core :as p]))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private env
  (client/default-env))

(def tool-spec
  {:name         "math_operation"
   :description  "Perform basic arithmetic operations"
   :input-schema [:map {:closed true}
                  [:a {:description "First number"} :int]
                  [:b {:description "Second number"} :int]
                  [:operation {:description "The operation to perform. One of 'add', 'subtract', 'multiply', 'divide'."}
                   [:enum "add" "subtract" "multiply" "divide"]]]})

(def ^:private test-image-base64
  (util/read-file-base64 "test/llx/ai/fixtures/test-image.png"))

(defn- extract-thinking
  [assistant-message]
  (->> (:content assistant-message)
       (filter #(= :thinking (:type %)))
       (map :thinking)
       (str/join "")))

(defn- execute-math
  [{:keys [a b operation]}]
  (case operation
    "add" (+ a b)
    "subtract" (- a b)
    "multiply" (* a b)
    "divide" (/ a b)
    0))

(defn- collect-stream!*
  [ch]
  (-> (util/collect-stream* ch 60000)
      (p/then (fn [{:keys [events result]}]
                (is (not (util/timeout-result? events)) "stream events should not time out")
                (is (map? result) "stream result should be an assistant message map")
                {:events events :result result}))))

(defn- basic-text-generation!*
  [model opts]
  (let [context {:system-prompt "You are a helpful assistant. Be concise."
                 :messages      [{:role      :user
                                  :content   "Reply with exactly: 'Hello test successful'"
                                  :timestamp (util/now-ms)}]}]
    (p/let [first-r (client/complete* env model context opts)]
      (is (= :assistant (:role first-r)))
      (is (seq (:content first-r)))
      (is (> (+ (get-in first-r [:usage :input] 0)
                (get-in first-r [:usage :cache-read] 0))
             0))
      (is (> (get-in first-r [:usage :output] 0) 0))
      (is (nil? (:error-message first-r)))
      (is (str/includes? (util/extract-text first-r) "Hello test successful"))

      (let [context2 {:system-prompt "You are a helpful assistant. Be concise."
                      :messages      [(first (:messages context))
                                      first-r
                                      {:role      :user
                                       :content   "Now say 'Goodbye test successful'"
                                       :timestamp (util/now-ms)}]}]
        (p/let [second-r (client/complete* env model context2 opts)]
          (is (= :assistant (:role second-r)))
          (is (seq (:content second-r)))
          (is (> (+ (get-in second-r [:usage :input] 0)
                    (get-in second-r [:usage :cache-read] 0))
                 0))
          (is (> (get-in second-r [:usage :output] 0) 0))
          (is (nil? (:error-message second-r)))
          (is (str/includes? (util/extract-text second-r) "Goodbye test successful"))
          true)))))

(defn- handle-tool-call!*
  [model opts]
  (p/let [{:keys [events result]} (collect-stream!*
                                   (client/stream* env model
                                                   {:system-prompt "You are a helpful assistant that uses tools when asked."
                                                    :messages      [{:role      :user
                                                                     :content   "Calculate 15 + 27 using the math_operation tool."
                                                                     :timestamp (util/now-ms)}]
                                                    :tools         [tool-spec]}
                                                   opts))]
    (let [types    (set (map :type events))
          tc-block (some #(when (= :tool-call (:type %)) %) (:content result))
          args     (:arguments tc-block)]
      (is (every? types #{:toolcall-start :toolcall-delta :toolcall-end})
          (str "expected toolcall-start/-delta/-end in event types, got " types))
      (is (= :tool-use (:stop-reason result))
          (str "expected :tool-use stop-reason, got " (:stop-reason result)))
      (is (some? tc-block) "result should contain a :tool-call content block")
      (is (= "math_operation" (:name tc-block)))
      (is (seq (:id tc-block)) "tool-call block should have a non-empty :id")
      (is (= 15 (:a args)) (str "expected a=15, got " (:a args)))
      (is (= 27 (:b args)) (str "expected b=27, got " (:b args)))
      (is (contains? #{"add" "subtract" "multiply" "divide"} (:operation args))
          (str "expected valid operation enum, got " (pr-str (:operation args))))
      true)))

(defn- handle-streaming!*
  [model opts]
  (p/let [{:keys [events result]} (collect-stream!*
                                   (client/stream* env model
                                                   {:system-prompt "You are a helpful assistant."
                                                    :messages      [{:role      :user
                                                                     :content   "Count from 1 to 3"
                                                                     :timestamp (util/now-ms)}]}
                                                   opts))]
    (let [types      (set (map :type events))
          text-accum (apply str (keep :text (filter #(= :text-delta (:type %)) events)))]
      (is (contains? types :text-start) "expected :text-start event")
      (is (contains? types :text-delta) "expected :text-delta event")
      (is (contains? types :text-end) "expected :text-end event")
      (is (pos? (count text-accum)) "accumulated text should be non-empty")
      (is (some #(= :text (:type %)) (:content result))
          "result should contain a :text content block")
      true)))

(defn- handle-thinking!*
  [model opts]
  (let [rand-num (rand-int 256)]
    (p/let [{:keys [events result]} (collect-stream!*
                                     (client/stream* env model
                                                     {:system-prompt "You are a helpful assistant."
                                                      :messages      [{:role      :user
                                                                       :content   (str "Think long and hard about " rand-num " + 27. Think step by step. Then output the result.")
                                                                       :timestamp (util/now-ms)}]}
                                                     opts))]
      (let [types          (set (map :type events))
            thinking-delta (apply str (keep :thinking (filter #(= :thinking-delta (:type %)) events)))
            thinking-final (extract-thinking result)]
        (is (= :stop (:stop-reason result))
            (str "expected :stop but got " (:stop-reason result) " error: " (:error-message result)))
        (is (contains? types :thinking-start) "expected :thinking-start event")
        (is (contains? types :thinking-end) "expected :thinking-end event")
        (if (contains? types :thinking-delta)
          (is (pos? (count thinking-delta)) "accumulated thinking from deltas should be non-empty")
          (is (or (pos? (count thinking-final))
                  (some #(= :thinking (:type %)) (:content result)))
              "when no :thinking-delta events are emitted, provider should still signal thinking in final content"))
        (is (or (contains? types :thinking-delta)
                (some #(= :thinking (:type %)) (:content result)))
            "thinking stream should contain either delta events or a final :thinking block")
        true))))

(defn- handle-image!*
  [model opts]
  (if-not (contains? (get-in model [:capabilities :input]) :image)
    (do
      (is true (str "Skipping image test - model " (:id model) " doesn't support images"))
      (p/resolved true))
    (p/let [result (client/complete* env model
                                     {:system-prompt "You are a helpful assistant."
                                      :messages      [{:role      :user
                                                       :content   [{:type :text
                                                                    :text "What do you see in this image? Please describe the shape (circle, rectangle, square, triangle, ...) and color (red, blue, green, ...). You MUST reply in English."}
                                                                   {:type      :image
                                                                    :data      test-image-base64
                                                                    :mime-type "image/png"}]
                                                       :timestamp (util/now-ms)}]}
                                     opts)]
      (let [text (str/lower-case (util/extract-text result))]
        (is (pos? (count (:content result))))
        (is (str/includes? text "green") "response should mention 'green'")
        (is (str/includes? text "triangle") "response should mention 'triangle'")
        true))))

(defn- multi-turn!*
  [model opts]
  (let [initial-msg   {:role      :user
                       :content   "Think about this briefly, then calculate 42 * 17 and 453 + 434 using the math_operation tool."
                       :timestamp (util/now-ms)}
        max-turns     5
        all-text      (atom "")
        seen-thinking (atom false)
        seen-tools    (atom false)]
    (-> (p/loop [messages [initial-msg]
                 turn     0]
          (if (>= turn max-turns)
            true
            (p/let [response (client/complete* env model
                                               {:system-prompt "You are a helpful assistant that can use tools to answer questions."
                                                :messages      messages
                                                :tools         [tool-spec]}
                                               opts)]
              (let [messages'    (conj messages response)
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
                                                   :timestamp    (util/now-ms)})))

                                      results))
                                  []
                                  (:content response))]
                (is (not= :error (:stop-reason response))
                    (str "unexpected error: " (:error-message response)))
                (if (= :stop (:stop-reason response))
                  true
                  (p/recur (into messages' tool-results)
                           (inc turn)))))))
        (p/then (fn [_]
                  (is (or @seen-thinking @seen-tools)
                      "should have seen thinking or tool calls")
                  (is (seq @all-text) "should have accumulated some text")
                  (is (str/includes? @all-text "714")
                      "accumulated text should contain 714 (42 * 17)")
                  (is (str/includes? @all-text "887")
                      "accumulated text should contain 887 (453 + 434)")
                  true)))))

(defn- run-provider-suite!*
  [{:keys [model opts thinking-opts]}]
  (p/let [_ (testing "basic-text-generation"
              (basic-text-generation!* model opts))
          _ (testing "handle-tool-call"
              (handle-tool-call!* model opts))
          _ (testing "handle-streaming"
              (handle-streaming!* model opts))
          _ (when thinking-opts
              (testing "handle-thinking"
                (handle-thinking!* model thinking-opts)))
          _ (testing "handle-image"
              (handle-image!* model opts))
          _ (when thinking-opts
              (testing "multi-turn"
                (multi-turn!* model thinking-opts)))]
    true))

(deftest ^:llx/google live-parity-google
  (util/async done
              (util/run-live-async!
               (run-provider-suite!* {:model         models/google
                                      :opts          {:max-output-tokens 256}
                                      :thinking-opts {:max-output-tokens 2048
                                                      :reasoning         {:level :high}}})
               done)))

(deftest ^:llx/openai live-parity-openai-completions
  (util/async done
              (util/run-live-async!
               (let [opts {:max-output-tokens 256}]
                 (p/let [_ (testing "basic-text-generation"
                             (basic-text-generation!* models/openai-completions opts))
                         _ (testing "handle-tool-call"
                             (handle-tool-call!* models/openai-completions opts))
                         _ (testing "handle-streaming"
                             (handle-streaming!* models/openai-completions opts))
                         _ (testing "handle-image"
                             (handle-image!* models/openai-completions opts))]
                   true))
               done)))

(deftest ^:llx/openai live-parity-openai-responses
  (util/async done
              (util/run-live-async!
               (run-provider-suite!* {:model         models/openai-responses
                                      :opts          {:max-output-tokens 256}
                                      :thinking-opts {:max-output-tokens 2048
                                                      :reasoning         {:effort :high}}})
               done)))

(deftest ^:llx/anthropic live-parity-anthropic
  (util/async done
              (util/run-live-async!
               (let [opts {:max-output-tokens 256}]
                 (p/let [_ (testing "basic-text-generation"
                             (basic-text-generation!* models/anthropic (assoc opts :reasoning {:level :high})))
                         _ (testing "handle-tool-call"
                             (handle-tool-call!* models/anthropic opts))
                         _ (testing "handle-streaming"
                             (handle-streaming!* models/anthropic opts))
                         _ (testing "handle-image"
                             (handle-image!* models/anthropic opts))]
                   true))
               done)))

(deftest ^:llx/mistral live-parity-mistral-devstral
  (util/async done
              (util/run-live-async!
               (let [opts {:max-output-tokens 256}]
                 (p/let [_ (testing "basic-text-generation"
                             (basic-text-generation!* models/mistral opts))
                         _ (testing "handle-tool-call"
                             (handle-tool-call!* models/mistral opts))
                         _ (testing "handle-streaming"
                             (handle-streaming!* models/mistral opts))
                         _ (testing "multi-turn"
                             (multi-turn!* models/mistral {:max-output-tokens 2048
                                                           :reasoning         {:level :medium}}))]
                   true))
               done)))

(deftest ^:llx/mistral live-parity-mistral-pixtral
  (util/async done
              (util/run-live-async!
               (let [opts {:max-output-tokens 256}]
                 (p/let [_ (testing "basic-text-generation"
                             (basic-text-generation!* models/mistral-pixtral opts))
                         _ (testing "handle-tool-call"
                             (handle-tool-call!* models/mistral-pixtral opts))
                         _ (testing "handle-streaming"
                             (handle-streaming!* models/mistral-pixtral opts))
                         _ (testing "handle-image"
                             (handle-image!* models/mistral-pixtral opts))]
                   true))
               done)))

(deftest ^:llx/ollama live-parity-ollama
  (util/async done
              (util/run-live-async!
               (run-provider-suite!* {:model         models/ollama
                                      :opts          {}
                                      :thinking-opts {:reasoning {:level :medium}}})
               done)))
