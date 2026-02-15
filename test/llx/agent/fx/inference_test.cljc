(ns llx.agent.fx.inference-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]]
       :cljs [[cljs.test :refer-macros [deftest is]]])
   [llx.agent.fx.inference :as sut]
   [llx.ai :as ai]))

(defn- zero-usage
  []
  {:input        0
   :output       0
   :cache-read   0
   :cache-write  0
   :total-tokens 0
   :cost         {:input 0 :output 0 :cache-read 0 :cache-write 0 :total 0}})

(defn- assistant-message
  [model text]
  {:role        :assistant
   :content     [{:type :text :text text}]
   :api         (:api model)
   :provider    (:provider model)
   :model       (:id model)
   :usage       (zero-usage)
   :stop-reason :stop
   :timestamp   1})

(deftest llm-event->step-start-initializes-partial-test
  (let [model                          (ai/get-model :openai "gpt-4o")
        {:keys [partial signal done?]} (sut/llm-event->step model nil {:type :start})]
    (is (false? done?))
    (is (= :assistant (:role partial)))
    (is (= [] (:content partial)))
    (is (= :llx.agent.signal/llm-start (:type signal)))
    (is (= partial (:message signal)))))

(deftest llm-event->step-text-delta-updates-chunk-test
  (let [model             (ai/get-model :openai "gpt-4o")
        {:keys [partial]} (sut/llm-event->step model nil {:type :start})
        {:keys [partial]} (sut/llm-event->step model partial {:type :text-start})
        step              (sut/llm-event->step model partial {:type :text-delta :text "Hello"})]
    (is (false? (:done? step)))
    (is (= :llx.agent.signal/llm-chunk (get-in step [:signal :type])))
    (is (= "Hello" (get-in step [:partial :content 0 :text])))))

(deftest llm-event->step-done-emits-assistant-message-test
  (let [model   (ai/get-model :openai "gpt-4o")
        message (assistant-message model "done")
        step    (sut/llm-event->step model nil {:type :done :assistant-message message})]
    (is (true? (:done? step)))
    (is (= :llx.agent.signal/llm-done (get-in step [:signal :type])))
    (is (= message (get-in step [:signal :message])))))

(deftest llm-event->step-rejects-non-canonical-events-test
  (let [model (ai/get-model :openai "gpt-4o")]
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (sut/llm-event->step model nil {:type "text_start"})))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (sut/llm-event->step model nil {:type :text-delta :delta "x"})))))
