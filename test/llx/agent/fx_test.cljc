(ns llx.agent.fx-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [deftest is testing]]])
   [llx.ai :as ai]
   [llx.agent.schema :as schema]
   [llx.agent.fx :as sut]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

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

(defn- stream-with-events
  [events]
  (let [ch (sp/chan :buf 16)]
    (doseq [event events]
      (sp/offer ch event))
    (sp/close ch)
    ch))

(defn- read-signals!
  [ch]
  #?(:clj
     (loop [acc []]
       (let [signal (sp/take! ch 200 ::timeout)]
         (cond
           (= ::timeout signal) (recur acc)
           (nil? signal) acc
           :else (recur (conj acc signal)))))
     :cljs
     []))

(defn- test-env
  [state overrides]
  (merge {:state_           (atom state)
          :command>         (sp/chan)
          :events-mx>       (sp/mult :buf (sp/sliding-buffer 16))
          :schema-registry  (schema/registry {})
          :convert-to-llm   identity
          :tools            {}
          :abort-signal     nil}
         overrides))

(deftest fx-call-llm-runs-hooks-and-maps-events-test
  (let [model                (ai/get-model :openai "gpt-4o")
        input-messages       [{:role :user :content "hello" :timestamp 1}]
        transformed-messages [{:role :user :content "trimmed" :timestamp 2}]
        llm-messages         [{:role :user :content "llm message" :timestamp 3}]
        final-message        (assistant-message model "Hello")
        seen*                (atom {})
        env                  (test-env
                              {:node               :node/streaming
                               :system-prompt      "sys"
                               :model              model
                               :thinking-level     :medium
                               :tools              [{:name "read_file"}]
                               :messages           input-messages
                               :stream-message     nil
                               :pending-tool-calls []
                               :error              nil
                               :steering-queue     #?(:clj clojure.lang.PersistentQueue/EMPTY
                                                      :cljs #queue [])
                               :follow-up-queue    #?(:clj clojure.lang.PersistentQueue/EMPTY
                                                      :cljs #queue [])
                               :steering-mode      :one-at-a-time
                               :follow-up-mode     :one-at-a-time}
                              {:convert-to-llm     (fn [messages]
                                                     (swap! seen* assoc :convert-input messages)
                                                     (p/delay 1 llm-messages))
                               :transform-context  (fn [messages signal]
                                                     (swap! seen* assoc :transform-input {:messages messages
                                                                                          :signal   signal})
                                                     (p/delay 1 transformed-messages))
                               :stream-fn          (fn [m context options]
                                                     (swap! seen* assoc :stream-input {:model   m
                                                                                       :context context
                                                                                       :options options})
                                                     (stream-with-events
                                                      [{:type :start}
                                                       {:type :text-start}
                                                       {:type :text-delta :text "Hel"}
                                                       {:type :text-delta :text "lo"}
                                                       {:type :done :assistant-message final-message}]))
                               :abort-signal       ::abort-signal
                               :session-id         "session-1"
                               :get-api-key        (fn [provider]
                                                     (swap! seen* assoc :get-api-key-provider provider)
                                                     (p/delay 1 "token"))
                               :thinking-budgets   {:high 1234}
                               :max-retry-delay-ms 2500})
        out                  (sut/execute-fx env {:fx/type :call-llm :messages input-messages})]
    #?(:clj
       (let [signals (read-signals! out)]
         (is (= [:signal/llm-start
                 :signal/llm-chunk
                 :signal/llm-chunk
                 :signal/llm-chunk
                 :signal/llm-done]
                (mapv :type signals)))
         (is (= "" (get-in (nth signals 1) [:chunk :content 0 :text])))
         (is (= "Hel" (get-in (nth signals 2) [:chunk :content 0 :text])))
         (is (= "Hello" (get-in (nth signals 3) [:chunk :content 0 :text])))
         (is (= final-message (:message (last signals))))
         (is (= {:messages input-messages
                 :signal   ::abort-signal}
                (:transform-input @seen*)))
         (is (= transformed-messages (:convert-input @seen*)))
         (is (= model (get-in @seen* [:stream-input :model])))
         (is (= {:system-prompt "sys"
                 :messages      llm-messages
                 :tools         [{:name "read_file"}]}
                (get-in @seen* [:stream-input :context])))
         (is (= :medium (get-in @seen* [:stream-input :options :reasoning])))
         (is (= ::abort-signal (get-in @seen* [:stream-input :options :signal])))
         (is (= "token" (get-in @seen* [:stream-input :options :api-key])))
         (is (= "openai" (:get-api-key-provider @seen*)))
         (is (= "session-1" (get-in @seen* [:stream-input :options :session-id])))
         (is (= {:high 1234} (get-in @seen* [:stream-input :options :thinking-budgets])))
         (is (= 2500 (get-in @seen* [:stream-input :options :max-retry-delay-ms])))
         (is (fn? (get-in @seen* [:stream-input :options :get-api-key]))))
       :cljs
       (is true))))

(deftest fx-call-llm-uses-default-stream-fn-when-not-provided-test
  (let [model  (ai/get-model :openai "gpt-4o")
        seen*  (atom nil)
        env    (test-env
                {:node               :node/streaming
                 :system-prompt      "sys"
                 :model              model
                 :thinking-level     :low
                 :tools              []
                 :messages           [{:role :user :content "x" :timestamp 1}]
                 :stream-message     nil
                 :pending-tool-calls []
                 :error              nil
                 :steering-queue     #?(:clj clojure.lang.PersistentQueue/EMPTY
                                        :cljs #queue [])
                 :follow-up-queue    #?(:clj clojure.lang.PersistentQueue/EMPTY
                                        :cljs #queue [])
                 :steering-mode      :one-at-a-time
                 :follow-up-mode     :one-at-a-time}
                {:convert-to-llm     identity
                 :abort-signal       ::abort-signal
                 :session-id         "session-default"
                 :get-api-key        (fn [_provider] "resolved-key")
                 :thinking-budgets   {:low 4321}
                 :max-retry-delay-ms 1500})
        events [{:type :start}
                {:type :done :assistant-message (assistant-message model "ok")}]]
    (with-redefs [ai/default-env (fn [] ::ai-env)
                  ai/stream      (fn [runtime-env stream-model context options]
                                   (reset! seen* {:runtime-env runtime-env
                                                  :model       stream-model
                                                  :context     context
                                                  :options     options})
                                   (stream-with-events events))]
      (let [out     (sut/execute-fx env {:fx/type :call-llm :messages [{:role :user :content "hi" :timestamp 2}]})
            signals #?(:clj (read-signals! out)
                       :cljs [])]
        #?(:clj
           (do
             (is (= [:signal/llm-start :signal/llm-done] (mapv :type signals)))
             (is (= ::ai-env (:runtime-env @seen*)))
             (is (= model (:model @seen*)))
             (is (= {:system-prompt "sys"
                     :messages      [{:role :user :content "hi" :timestamp 2}]
                     :tools         []}
                    (:context @seen*)))
             (is (= :low (get-in @seen* [:options :reasoning])))
             (is (= ::abort-signal (get-in @seen* [:options :signal])))
             (is (= "session-default" (get-in @seen* [:options :session-id])))
             (is (= "resolved-key" (get-in @seen* [:options :api-key])))
             (is (= {:low 4321} (get-in @seen* [:options :thinking-budgets])))
             (is (= 1500 (get-in @seen* [:options :max-retry-delay-ms])))
             (is (nil? (get-in @seen* [:options :get-api-key]))))
           :cljs
           (is true))))))

(deftest fx-call-llm-emits-llm-error-on-hook-failure-test
  (let [model (ai/get-model :openai "gpt-4o")
        env   (test-env
               {:node               :node/streaming
                :system-prompt      "sys"
                :model              model
                :thinking-level     :off
                :tools              []
                :messages           []
                :stream-message     nil
                :pending-tool-calls []
                :error              nil
                :steering-queue     #?(:clj clojure.lang.PersistentQueue/EMPTY
                                       :cljs #queue [])
                :follow-up-queue    #?(:clj clojure.lang.PersistentQueue/EMPTY
                                       :cljs #queue [])
                :steering-mode      :one-at-a-time
                :follow-up-mode     :one-at-a-time}
               {:convert-to-llm (fn [_] (throw (ex-info "boom" {:type ::hook-failed})))})
        out   (sut/execute-fx env {:fx/type :call-llm :messages [{:role :user :content "hi" :timestamp 1}]})]
    #?(:clj
       (let [signals (read-signals! out)]
         (is (= 1 (count signals)))
         (is (= :signal/llm-error (:type (first signals))))
         (is (= ::hook-failed (-> signals first :error ex-data :type))))
       :cljs
       (is true))))

(deftest execute-fx-validates-effect-shape-test
  (let [model (ai/get-model :openai "gpt-4o")
        env   (test-env {:node               :node/streaming
                         :system-prompt      "sys"
                         :model              model
                         :thinking-level     :off
                         :tools              []
                         :messages           []
                         :stream-message     nil
                         :pending-tool-calls []
                         :error              nil
                         :steering-queue     #?(:clj clojure.lang.PersistentQueue/EMPTY
                                                :cljs #queue [])
                         :follow-up-queue    #?(:clj clojure.lang.PersistentQueue/EMPTY
                                                :cljs #queue [])
                         :steering-mode      :one-at-a-time
                         :follow-up-mode     :one-at-a-time}
                        {})]
    #?(:clj
       (do
         (is (thrown? Exception
                      (sut/execute-fx env {:fx/type :call-llm})))
         (is (thrown? Exception
                      (sut/execute-fx env {:fx/type :unknown}))))
       :cljs
       (is true))))
