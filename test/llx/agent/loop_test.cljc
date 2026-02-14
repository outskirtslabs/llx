(ns llx.agent.loop-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]
             [llx.ai.test-util :as util]]
       :cljs [[cljs.test :refer-macros [deftest is]]
              [llx.ai.test-util :as util :include-macros true]])
   [llx.agent.loop :as sut]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(def base-model
  {:id             "gpt-4o-mini"
   :name           "GPT-4o Mini"
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(defn- now-ms
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

(defn- user-message
  [text]
  {:role      :user
   :content   [{:type :text :text text}]
   :timestamp (now-ms)})

(defn- assistant-message
  [content stop-reason]
  {:role        :assistant
   :content     content
   :api         :openai-completions
   :provider    :openai
   :model       "gpt-4o-mini"
   :usage       {:input        0
                 :output       0
                 :cache-read   0
                 :cache-write  0
                 :total-tokens 0
                 :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
   :stop-reason stop-reason
   :timestamp   (now-ms)})

(defn- channel-from-events
  [events]
  (let [ch (sp/chan 32)]
    (-> (reduce
         (fn [acc event]
           (p/then acc
                   (fn [_]
                     (sp/put ch event))))
         (p/resolved true)
         events)
        (p/finally (fn [_ _]
                     (sp/close ch))))
    ch))

(deftest agent-loop-emits-basic-event-sequence
  (util/async done
              (let [prompt    (user-message "hello")
                    assistant (assistant-message [{:type :text :text "hi"}] :stop)
                    stream-fn (fn [_env _model _context _opts]
                                (channel-from-events [{:type :start}
                                                      {:type :text-start}
                                                      {:type :text-delta :text "hi"}
                                                      {:type :text-end}
                                                      {:type :done :assistant-message assistant}]))
                    context   {:system-prompt ""
                               :messages      []
                               :tools         []}
                    config    {:env            {}
                               :model          base-model
                               :convert-to-llm identity
                               :stream-fn      stream-fn}
                    events-ch (sut/agent-loop [prompt] context config)]
                (-> (util/collect-events* events-ch 2000)
                    (p/then (fn [events]
                              (let [types (mapv :llx.agent.event/type events)]
                                (is (= :agent-start (first types)))
                                (is (= :agent-end (last types)))
                                (is (some #{:message-update} types))
                                (is (some #{:turn-end} types)))
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest agent-loop-continue-rejects-empty-context
  (is (thrown-with-msg?
       #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
       #"Cannot continue: no messages in context"
       (sut/agent-loop-continue {:system-prompt "" :messages [] :tools []}
                                {:env {} :model base-model :convert-to-llm identity}))))

(deftest agent-loop-skips-remaining-tool-calls-on-steering
  (util/async done
              (let [executed*        (atom [])
                    stream-call*     (atom 0)
                    steering-once*   (atom false)
                    prompt           (user-message "run tools")
                    steering-message (user-message "interrupt")
                    tool             {:name         "echo"
                                      :label        "Echo"
                                      :description  "Echo tool"
                                      :input-schema [:map [:value :string]]
                                      :execute      (fn [_tool-call-id params _signal _on-update]
                                                      (swap! executed* conj (:value params))
                                                      {:content [{:type :text :text (str "ok:" (:value params))}]
                                                       :details {:value (:value params)}})}
                    first-assistant  (assistant-message [{:type      :tool-call
                                                          :id        "tool-1"
                                                          :name      "echo"
                                                          :arguments {:value "first"}}
                                                         {:type      :tool-call
                                                          :id        "tool-2"
                                                          :name      "echo"
                                                          :arguments {:value "second"}}]
                                                        :tool-use)
                    final-assistant  (assistant-message [{:type :text :text "done"}] :stop)
                    stream-fn        (fn [_env _model _context _opts]
                                       (let [idx (swap! stream-call* inc)]
                                         (if (= idx 1)
                                           (channel-from-events [{:type :start}
                                                                 {:type              :done
                                                                  :assistant-message first-assistant}])
                                           (channel-from-events [{:type :start}
                                                                 {:type              :done
                                                                  :assistant-message final-assistant}]))))
                    context          {:system-prompt ""
                                      :messages      []
                                      :tools         [tool]}
                    config           {:env                   {}
                                      :model                 base-model
                                      :convert-to-llm        identity
                                      :stream-fn             stream-fn
                                      :get-steering-messages (fn []
                                                               (if (and (= 1 (count @executed*))
                                                                        (compare-and-set! steering-once* false true))
                                                                 [steering-message]
                                                                 []))}
                    events-ch        (sut/agent-loop [prompt] context config)]
                (-> (util/collect-events* events-ch 3000)
                    (p/then (fn [events]
                              (is (= ["first"] @executed*))
                              (let [tool-end-events
                                    (filter #(= :tool-execution-end (:llx.agent.event/type %))
                                            events)
                                    interrupted?
                                    (some #(and (= :message-start (:llx.agent.event/type %))
                                                (= steering-message (:llx.agent.event/message %)))
                                          events)]
                                (is (= 2 (count tool-end-events)))
                                (is (true? (:llx.agent.event/error? (second tool-end-events))))
                                (is interrupted?))
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))
