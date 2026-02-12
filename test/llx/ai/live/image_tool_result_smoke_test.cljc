(ns llx.ai.live.image-tool-result-smoke-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]]
       :cljs [[cljs.test :refer [deftest is]]])
   #?@(:clj [[llx.ai.test-util :as util]]
       :cljs [[llx.ai.test-util :as util :include-macros true]])
   [clojure.string :as str]
   [llx.ai :as client]
   [llx.ai.live.models :as models]
   [promesa.core :as p]))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private env
  (client/default-env))

(def ^:private test-image-base64
  (util/read-file-base64 "test/llx/ai/fixtures/test-image.png"))

(defn- assert-image-described!
  [assistant-message]
  (let [text (-> assistant-message
                 util/extract-text
                 str/lower-case)]
    (is (or (str/includes? text "green") (str/includes? text "triangle"))
        (str "expected image description to mention 'green' or 'triangle', got: "
             (subs text 0 (min 200 (count text)))))))

(defn- image-only-context
  [model]
  {:system-prompt "You are a helpful assistant."
   :messages      [{:role      :user
                    :content   "Use the get_screenshot tool to capture a screenshot."
                    :timestamp (util/now-ms)}
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
                    :timestamp   (util/now-ms)}
                   {:role         :tool-result
                    :tool-call-id "call_img_1"
                    :tool-name    "get_screenshot"
                    :content      [{:type :image :data test-image-base64 :mime-type "image/png"}]
                    :is-error?    false
                    :timestamp    (util/now-ms)}
                   {:role      :user
                    :content   "What do you see in the screenshot? Describe the shape and color."
                    :timestamp (util/now-ms)}]})

(defn- text-and-image-context
  [model]
  {:system-prompt "You are a helpful assistant."
   :messages      [{:role      :user
                    :content   "Use the get_screenshot tool."
                    :timestamp (util/now-ms)}
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
                    :timestamp   (util/now-ms)}
                   {:role         :tool-result
                    :tool-call-id "call_img_2"
                    :tool-name    "get_screenshot"
                    :content      [{:type :text :text "Here is the screenshot of the canvas."}
                                   {:type :image :data test-image-base64 :mime-type "image/png"}]
                    :is-error?    false
                    :timestamp    (util/now-ms)}
                   {:role      :user
                    :content   "What do you see in the screenshot? Describe the shape and color."
                    :timestamp (util/now-ms)}]})

(defn- assert-image-result*
  [result]
  (is (= :assistant (:role result)))
  (is (= :stop (:stop-reason result))
      (str "expected :stop, got " (:stop-reason result) " error=" (:error-message result)))
  (assert-image-described! result)
  true)

(defn- test-tool-result-image-only!*
  [model opts]
  (-> (client/complete* env model (image-only-context model) opts)
      (p/then assert-image-result*)))

(defn- test-tool-result-text-and-image!*
  [model opts]
  (-> (client/complete* env model (text-and-image-context model) opts)
      (p/then assert-image-result*)))

(defn- run-image-suite!*
  [model opts]
  (p/let [_ (test-tool-result-image-only!* model opts)
          _ (test-tool-result-text-and-image!* model opts)]
    true))

(deftest ^:llx/openai live-image-tool-result-openai-completions
  (util/async done
              (util/run-live-async!
               (run-image-suite!* models/openai-completions {})
               done)))

(deftest ^:llx/openai live-image-tool-result-openai-responses
  (util/async done
              (util/run-live-async!
               (run-image-suite!* models/openai-responses {})
               done)))

(deftest ^:llx/anthropic live-image-tool-result-anthropic
  (util/async done
              (util/run-live-async!
               (run-image-suite!* models/anthropic {})
               done)))

(deftest ^:llx/google live-image-tool-result-google
  (util/async done
              (util/run-live-async!
               (run-image-suite!* models/google {})
               done)))
