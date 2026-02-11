(ns llx.ai.live.image-tool-result-smoke-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [llx.ai :as client]
   [llx.ai.live.env :as live-env]
   [llx.ai.live.models :as models]
   [llx.ai.impl.utils.await :as await])
  (:import
   [java.io File]
   [java.util Base64]))

(set! *warn-on-reflection* true)

(def ^:private env
  (client/default-env))

(def ^:private test-image-base64
  (let [f (File. "test/llx/ai/fixtures/test-image.png")]
    (.encodeToString (Base64/getEncoder) (-> f .toPath java.nio.file.Files/readAllBytes))))

(defn- extract-text
  [assistant-message]
  (->> (:content assistant-message)
       (filter #(= :text (:type %)))
       (map :text)
       (str/join "")
       str/lower-case))

(defn- assert-image-described!
  [assistant-message]
  (let [text (extract-text assistant-message)]
    (is (or (str/includes? text "green") (str/includes? text "triangle"))
        (str "expected image description to mention 'green' or 'triangle', got: "
             (subs text 0 (min 200 (count text)))))))

(defn- test-tool-result-image-only!
  [model opts]
  (let [context {:system-prompt "You are a helpful assistant."
                 :messages      [{:role      :user
                                  :content   "Use the get_screenshot tool to capture a screenshot."
                                  :timestamp (System/currentTimeMillis)}
                                 {:role        :assistant
                                  :content     [{:type      :tool-call
                                                 :id        "call_img_1"
                                                 :name      "get_screenshot"
                                                 :arguments {}}]
                                  :api         (:api model)
                                  :provider    (:provider model)
                                  :model       (:id model)
                                  :usage       {:input 1                                                                    :output 1 :cache-read 0 :cache-write 0 :total-tokens 2
                                                :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                                  :stop-reason :tool-use
                                  :timestamp   (System/currentTimeMillis)}
                                 {:role         :tool-result
                                  :tool-call-id "call_img_1"
                                  :tool-name    "get_screenshot"
                                  :content      [{:type :image :data test-image-base64 :mime-type "image/png"}]
                                  :is-error?    false
                                  :timestamp    (System/currentTimeMillis)}
                                 {:role      :user
                                  :content   "What do you see in the screenshot? Describe the shape and color."
                                  :timestamp (System/currentTimeMillis)}]}
        result  (await/await! (client/complete* env model context opts))]
    (is (= :assistant (:role result)))
    (is (= :stop (:stop-reason result))
        (str "expected :stop, got " (:stop-reason result) " error=" (:error-message result)))
    (assert-image-described! result)))

(defn- test-tool-result-text-and-image!
  [model opts]
  (let [context {:system-prompt "You are a helpful assistant."
                 :messages      [{:role      :user
                                  :content   "Use the get_screenshot tool."
                                  :timestamp (System/currentTimeMillis)}
                                 {:role        :assistant
                                  :content     [{:type      :tool-call
                                                 :id        "call_img_2"
                                                 :name      "get_screenshot"
                                                 :arguments {}}]
                                  :api         (:api model)
                                  :provider    (:provider model)
                                  :model       (:id model)
                                  :usage       {:input 1                                                                    :output 1 :cache-read 0 :cache-write 0 :total-tokens 2
                                                :cost  {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
                                  :stop-reason :tool-use
                                  :timestamp   (System/currentTimeMillis)}
                                 {:role         :tool-result
                                  :tool-call-id "call_img_2"
                                  :tool-name    "get_screenshot"
                                  :content      [{:type :text :text "Here is the screenshot of the canvas."}
                                                 {:type :image :data test-image-base64 :mime-type "image/png"}]
                                  :is-error?    false
                                  :timestamp    (System/currentTimeMillis)}
                                 {:role      :user
                                  :content   "What do you see in the screenshot? Describe the shape and color."
                                  :timestamp (System/currentTimeMillis)}]}
        result  (await/await! (client/complete* env model context opts))]
    (is (= :assistant (:role result)))
    (is (= :stop (:stop-reason result))
        (str "expected :stop, got " (:stop-reason result) " error=" (:error-message result)))
    (assert-image-described! result)))

(deftest live-image-tool-result-openai-completions
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (testing "tool-result image only"
      (test-tool-result-image-only! models/openai-completions {:api-key api-key}))
    (testing "tool-result text and image"
      (test-tool-result-text-and-image! models/openai-completions {:api-key api-key}))))

(deftest live-image-tool-result-openai-responses
  (let [api-key (live-env/get-env "OPENAI_API_KEY")]
    (testing "tool-result image only"
      (test-tool-result-image-only! models/openai-responses {:api-key api-key}))
    (testing "tool-result text and image"
      (test-tool-result-text-and-image! models/openai-responses {:api-key api-key}))))

(deftest live-image-tool-result-anthropic
  (let [api-key (live-env/get-env "ANTHROPIC_API_KEY")]
    (testing "tool-result image only"
      (test-tool-result-image-only! models/anthropic {:api-key api-key}))
    (testing "tool-result text and image"
      (test-tool-result-text-and-image! models/anthropic {:api-key api-key}))))

(deftest live-image-tool-result-google
  (let [api-key (live-env/get-env "GEMINI_API_KEY")]
    (testing "tool-result image only"
      (test-tool-result-image-only! models/google {:api-key api-key}))
    (testing "tool-result text and image"
      (test-tool-result-text-and-image! models/google {:api-key api-key}))))
