(ns llx-ai.live.provider-parity-smoke-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [llx-ai.client.jvm :as client]
   [llx-ai.event-stream :as event-stream]
   [llx-ai.live.env :as live-env]))

(set! *warn-on-reflection* true)

(def ^:private deep-parity-flag "LLX_LIVE_DEEP_PARITY")
(def ^:private image-parity-flag "LLX_LIVE_IMAGE_PARITY")

(defn- deep-parity-enabled?
  []
  (= "1" (live-env/get-env deep-parity-flag)))

(defn- image-parity-enabled?
  []
  (= "1" (live-env/get-env image-parity-flag)))

(def tool-spec
  {:name         "math_operation"
   :description  "Performs an arithmetic operation."
   :input-schema [:map {:closed true}
                  [:a :int]
                  [:b :int]
                  [:operation [:enum "add" "subtract" "multiply" "divide"]]]})

(defn- contains-tool-call?
  [assistant-message]
  (some #(= :tool-call (:type %)) (:content assistant-message)))

(defn- stream-tool-call-check!
  [model opts]
  (let [stream (client/stream model
                              {:messages [{:role      :user
                                           :content   "Use the math_operation tool to calculate 15 + 27."
                                           :timestamp 1}]
                               :tools    [tool-spec]}
                              opts)
        events (event-stream/drain! stream)
        out    (event-stream/result stream)
        types  (mapv :type events)]
    (is (= :start (first types)))
    (is (some #{:toolcall-start} types))
    (is (some #{:toolcall-delta} types))
    (is (some #{:toolcall-end} types))
    (is (#{:tool-use :stop} (:stop-reason out)))
    (is (contains-tool-call? out))))

(defn- stream-thinking-check!
  [model opts]
  (let [stream (client/stream model
                              {:messages [{:role      :user
                                           :content   "Think step by step about 31 + 14, then output only the result."
                                           :timestamp 1}]}
                              opts)
        events (event-stream/drain! stream)
        out    (event-stream/result stream)
        types  (set (map :type events))]
    (is (or (contains? types :thinking-start)
            (contains? types :thinking-delta)
            (some #(= :thinking (:type %)) (:content out))))
    (is (#{:stop :length} (:stop-reason out)))))

(def ^:private red-pixel-base64
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADUlEQVR42mP8z/C/HwAF/gL+q5K56QAAAABJRU5ErkJggg==")

(defn- image-input-check!
  [model opts]
  (let [out  (client/complete model
                              {:messages [{:role      :user
                                           :content   [{:type :text
                                                        :text "Describe this image in English with exactly two words containing red."}
                                                       {:type      :image
                                                        :data      red-pixel-base64
                                                        :mime-type "image/png"}]
                                           :timestamp 1}]}
                              opts)
        text (->> (:content out)
                  (filter #(= :text (:type %)))
                  (map :text)
                  (str/join " ")
                  str/lower-case)]
    (is (#{:stop :length} (:stop-reason out)))
    (is (str/includes? text "red"))))

(defn- follow-up-check!
  [model opts]
  (let [context  {:messages [{:role      :user
                              :content   "Reply with exactly: followup-one"
                              :timestamp 1}]}
        first-r  (client/complete model context opts)
        context2 {:messages [{:role      :user
                              :content   "Reply with exactly: followup-one"
                              :timestamp 1}
                             first-r
                             {:role      :user
                              :content   "Reply with exactly: followup-two"
                              :timestamp 2}]}
        second-r (client/complete model context2 opts)
        text-2   (->> (:content second-r)
                      (filter #(= :text (:type %)))
                      (map :text)
                      (str/join " "))]
    (is (#{:stop :length} (:stop-reason first-r)))
    (is (#{:stop :length} (:stop-reason second-r)))
    (is (str/includes? (str/lower-case text-2) "followup-two"))))

(defn- maybe-openai-key
  []
  (live-env/get-env "OPENAI_API_KEY"))

(defn- maybe-anthropic-key
  []
  (live-env/get-env "ANTHROPIC_API_KEY"))

(defn- maybe-google-key
  []
  (live-env/get-env "GEMINI_API_KEY"))

(defn- maybe-mistral-key
  []
  (live-env/get-env "MISTRAL_API_KEY"))

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
   :capabilities   {:reasoning? false :input #{:text}}})

(def anthropic-model
  {:id             (or (live-env/get-env "LLX_LIVE_ANTHROPIC_MODEL") "claude-sonnet-4-5")
   :name           "Anthropic parity model"
   :provider       :anthropic
   :api            :anthropic-messages
   :base-url       (or (live-env/get-env "LLX_LIVE_ANTHROPIC_BASE_URL") "https://api.anthropic.com")
   :context-window 200000
   :max-tokens     8192
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? true :input #{:text :image}}})

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

(def ollama-model
  {:id             (or (live-env/get-env "LLX_LIVE_OLLAMA_MODEL") "devstral-small-2:latest")
   :name           "OpenAI-compatible parity model"
   :provider       :openai-compatible
   :api            :openai-completions
   :base-url       (or (live-env/get-env "LLX_LIVE_OLLAMA_BASE_URL") "http://localhost:11434/v1")
   :context-window 32768
   :max-tokens     4096
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(deftest live-parity-openai-responses-followup-and-tool
  (let [api-key (maybe-openai-key)]
    (if-not (and (deep-parity-enabled?) api-key)
      (is true (str "Skipping deep parity OpenAI Responses test: set "
                    deep-parity-flag "=1 and OPENAI_API_KEY"))
      (testing "OpenAI Responses supports follow-up continuity and tool stream events"
        (follow-up-check! openai-responses-model {:api-key   api-key                                         :max-output-tokens 128
                                                  :reasoning {:level :high :effort :high :summary :detailed}})
        (stream-tool-call-check! openai-responses-model {:api-key   api-key                                         :max-output-tokens 128
                                                         :reasoning {:level :high :effort :high :summary :detailed}})))))

(deftest live-parity-openai-responses-thinking-and-image
  (let [api-key (maybe-openai-key)]
    (if-not (and (deep-parity-enabled?) api-key)
      (is true (str "Skipping deep parity OpenAI Responses thinking/image test: set "
                    deep-parity-flag "=1 and OPENAI_API_KEY"))
      (testing "OpenAI Responses supports thinking stream and image input"
        (stream-thinking-check! openai-responses-model {:api-key   api-key                                         :max-output-tokens 128
                                                        :reasoning {:level :high :effort :high :summary :detailed}})
        (if (image-parity-enabled?)
          (image-input-check! openai-responses-model {:api-key api-key :max-output-tokens 128})
          (is true (str "Skipping image parity check: set " image-parity-flag "=1")))))))

(deftest live-parity-openai-completions-followup-and-tool
  (let [api-key (maybe-openai-key)]
    (if-not (and (deep-parity-enabled?) api-key)
      (is true (str "Skipping deep parity OpenAI Completions test: set "
                    deep-parity-flag "=1 and OPENAI_API_KEY"))
      (testing "OpenAI Completions supports follow-up continuity and tool stream events"
        (follow-up-check! openai-completions-model {:api-key api-key :max-output-tokens 128})
        (stream-tool-call-check! openai-completions-model {:api-key api-key :max-output-tokens 128})))))

(deftest live-parity-anthropic-followup-and-tool
  (let [api-key (maybe-anthropic-key)]
    (if-not (and (deep-parity-enabled?) api-key)
      (is true (str "Skipping deep parity Anthropic test: set "
                    deep-parity-flag "=1 and ANTHROPIC_API_KEY"))
      (testing "Anthropic supports follow-up continuity and tool stream events"
        (follow-up-check! anthropic-model {:api-key api-key :max-output-tokens 128})
        (stream-tool-call-check! anthropic-model {:api-key api-key :max-output-tokens 128})))))

(deftest live-parity-google-followup-and-tool
  (let [api-key (maybe-google-key)]
    (if-not (and (deep-parity-enabled?) api-key)
      (is true (str "Skipping deep parity Google test: set "
                    deep-parity-flag "=1 and GEMINI_API_KEY"))
      (testing "Google supports follow-up continuity and tool stream events"
        (follow-up-check! google-model {:api-key   api-key                                         :max-output-tokens 128
                                        :reasoning {:level :high :effort :high :summary :detailed}})
        (stream-tool-call-check! google-model {:api-key   api-key                                         :max-output-tokens 128
                                               :reasoning {:level :high :effort :high :summary :detailed}})))))

(deftest live-parity-google-thinking-and-image
  (let [api-key (maybe-google-key)]
    (if-not (and (deep-parity-enabled?) api-key)
      (is true (str "Skipping deep parity Google thinking/image test: set "
                    deep-parity-flag "=1 and GEMINI_API_KEY"))
      (testing "Google supports thinking stream and image input"
        (stream-thinking-check! google-model {:api-key   api-key                                         :max-output-tokens 128
                                              :reasoning {:level :high :effort :high :summary :detailed}})
        (if (image-parity-enabled?)
          (image-input-check! google-model {:api-key   api-key                                         :max-output-tokens 128
                                            :reasoning {:level :high :effort :high :summary :detailed}})
          (is true (str "Skipping image parity check: set " image-parity-flag "=1")))))))

(deftest live-parity-mistral-followup-and-tool
  (let [api-key (maybe-mistral-key)]
    (if-not (and (deep-parity-enabled?) api-key)
      (is true (str "Skipping deep parity Mistral test: set "
                    deep-parity-flag "=1 and MISTRAL_API_KEY"))
      (testing "Mistral supports follow-up continuity and tool stream events"
        (follow-up-check! mistral-model {:api-key api-key :max-output-tokens 128})
        (stream-tool-call-check! mistral-model {:api-key api-key :max-output-tokens 128})))))

(deftest live-parity-openai-compatible-followup-and-tool
  (if-not (deep-parity-enabled?)
    (is true (str "Skipping deep parity OpenAI-compatible test: set " deep-parity-flag "=1"))
    (testing "OpenAI-compatible supports follow-up continuity and tool stream events"
      (follow-up-check! ollama-model {:max-output-tokens 128 :temperature 0.0})
      (stream-tool-call-check! ollama-model {:max-output-tokens 128 :temperature 0.0}))))
