(ns llx.agent.schema-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [deftest is testing]]])
   [llx.agent.schema :as sut]
   [promesa.core :as p]))

(def valid-model
  {:id             "gpt-5"
   :name           "GPT-5"
   :provider       :openai
   :api            :openai-responses
   :base-url       "https://api.openai.com/v1"
   :context-window 400000
   :max-tokens     16000
   :cost           {:input 1.25 :output 10.0 :cache-read 0.125 :cache-write 1.25}
   :capabilities   {:reasoning? true :input #{:text :image} :supports-xhigh? true}})

(def valid-usage
  {:input        10
   :output       5
   :cache-read   0
   :cache-write  0
   :total-tokens 15
   :cost         {:input 0.01 :output 0.02 :cache-read 0.0 :cache-write 0.0 :total 0.03}})

(def valid-user-message
  {:role      :user
   :content   [{:type :text :text "hello"}]
   :timestamp 1730000000000})

(def valid-assistant-message
  {:role        :assistant
   :content     [{:type :text :text "hi"}
                 {:type :thinking :thinking "thinking"}
                 {:type :tool-call :id "call_1" :name "search" :arguments {:q "x"}}]
   :api         :openai-responses
   :provider    :openai
   :model       "gpt-5"
   :usage       valid-usage
   :stop-reason :stop
   :timestamp   1730000000001})

(def valid-tool-result-message
  {:role         :tool-result
   :tool-call-id "call_1"
   :tool-name    "search"
   :content      [{:type :text :text "result"}]
   :is-error?    false
   :timestamp    1730000000002})

(def valid-tool
  {:name         "search"
   :label        "Search"
   :description  "Search docs"
   :input-schema [:map [:q :string]]
   :execute      (fn [_tool-call-id _args _signal _on-update]
                   {:content [{:type :text :text "ok"}] :details {:source :unit}})})

(deftest thinking-level-schema
  (is (sut/valid? :llx.agent/thinking-level :off))
  (is (sut/valid? :llx.agent/thinking-level :xhigh))
  (is (not (sut/valid? :llx.agent/thinking-level :extreme))))

(deftest core-contract-schemas
  (testing "tool result"
    (is (sut/valid? :llx.agent/tool-result
                    {:content [{:type :text :text "done"}] :details {:foo :bar}}))
    (is (not (sut/valid? :llx.agent/tool-result {:content []}))))

  (testing "tool"
    (is (sut/valid? :llx.agent/tool valid-tool))
    (is (not (sut/valid? :llx.agent/tool (dissoc valid-tool :execute)))))

  (testing "message union"
    (is (sut/valid? :llx.agent/message valid-user-message))
    (is (sut/valid? :llx.agent/message valid-assistant-message))
    (is (sut/valid? :llx.agent/message valid-tool-result-message))
    (is (sut/valid? :llx.agent/message {:role :ui-note :text "custom" :id "n1"}))
    (is (not (sut/valid? :llx.agent/message {:role :assistant :content []}))))

  (testing "context"
    (is (sut/valid? :llx.agent/context {:system-prompt "You are helpful"
                                        :messages      [valid-user-message]
                                        :tools         [valid-tool]}))
    (is (not (sut/valid? :llx.agent/context {:messages []})))))

(deftest state-config-streamfn-schemas
  (is (sut/valid? :llx.agent/state
                  {:system-prompt      "You are helpful"
                   :model              valid-model
                   :thinking-level     :off
                   :tools              [valid-tool]
                   :messages           [valid-user-message]
                   :streaming?         false
                   :stream-message     nil
                   :pending-tool-calls #{"call_1"}
                   :error              nil}))

  (is (sut/valid? :llx.agent/stream-fn (fn [_model _context _opts])))

  (is (sut/valid? :llx.agent/loop-config
                  {:model          valid-model
                   :convert-to-llm (fn [msgs] msgs)
                   :reasoning      :low
                   :max-tokens     512
                   :metadata       {:trace-id "abc"}}))

  (is (not (sut/valid? :llx.agent/loop-config
                       {:model          valid-model
                        :convert-to-llm :not-a-fn}))))

(deftest event-schemas
  (is (sut/valid? :llx.agent/event-agent-start {:llx.agent.event/type :agent-start}))
  (is (sut/valid? :llx.agent/event-agent-end {:llx.agent.event/type     :agent-end
                                              :llx.agent.event/messages [valid-user-message]}))
  (is (sut/valid? :llx.agent/event-turn-start {:llx.agent.event/type :turn-start}))
  (is (sut/valid? :llx.agent/event-turn-end
                  {:llx.agent.event/type         :turn-end
                   :llx.agent.event/message      valid-assistant-message
                   :llx.agent.event/tool-results [valid-tool-result-message]}))
  (is (sut/valid? :llx.agent/event-message-start
                  {:llx.agent.event/type    :message-start
                   :llx.agent.event/message valid-user-message}))
  (is (sut/valid? :llx.agent/event-message-update
                  {:llx.agent.event/type                    :message-update
                   :llx.agent.event/message                 valid-assistant-message
                   :llx.agent.event/assistant-message-event {:type :text-delta :text "h"}}))
  (is (sut/valid? :llx.agent/event-message-end
                  {:llx.agent.event/type    :message-end
                   :llx.agent.event/message valid-assistant-message}))
  (is (sut/valid? :llx.agent/event-tool-execution-start
                  {:llx.agent.event/type         :tool-execution-start
                   :llx.agent.event/tool-call-id "call_1"
                   :llx.agent.event/tool-name    "search"
                   :llx.agent.event/args         {:q "x"}}))
  (is (sut/valid? :llx.agent/event-tool-execution-update
                  {:llx.agent.event/type           :tool-execution-update
                   :llx.agent.event/tool-call-id   "call_1"
                   :llx.agent.event/tool-name      "search"
                   :llx.agent.event/args           {:q "x"}
                   :llx.agent.event/partial-result {:content [{:type :text :text "p"}]
                                                    :details {:pct 50}}}))
  (is (sut/valid? :llx.agent/event-tool-execution-end
                  {:llx.agent.event/type         :tool-execution-end
                   :llx.agent.event/tool-call-id "call_1"
                   :llx.agent.event/tool-name    "search"
                   :llx.agent.event/result       {:content [{:type :text :text "ok"}]
                                                  :details {:ms 5}}
                   :llx.agent.event/error?       false}))
  (is (sut/valid? :llx.agent/event {:llx.agent.event/type :agent-start}))
  (is (not (sut/valid? :llx.agent/event {:llx.agent.event/type :turn-end}))))

(deftest runtime-command-schemas
  (is (sut/valid? :llx.agent/command-type :llx.agent.command/prompt))
  (is (sut/valid? :llx.agent/command-type :llx.agent.command/shutdown))
  (is (not (sut/valid? :llx.agent/command-type :prompt)))

  (is (sut/valid? :llx.agent/command
                  {:llx.agent.command/type :llx.agent.command/prompt
                   :messages               valid-user-message}))
  (is (sut/valid? :llx.agent/command
                  {:llx.agent.command/type :llx.agent.command/prompt
                   :messages               [valid-user-message]}))
  (is (sut/valid? :llx.agent/command
                  {:llx.agent.command/type :llx.agent.command/continue}))
  (is (sut/valid? :llx.agent/command
                  {:llx.agent.command/type :llx.agent.command/steer
                   :messages               valid-user-message}))
  (is (sut/valid? :llx.agent/command
                  {:llx.agent.command/type :llx.agent.command/follow-up
                   :messages               [valid-user-message]}))
  (is (sut/valid? :llx.agent/command
                  {:llx.agent.command/type :llx.agent.command/abort}))
  (is (sut/valid? :llx.agent/command
                  {:llx.agent.command/type :llx.agent.command/reset}))
  (is (sut/valid? :llx.agent/command
                  {:llx.agent.command/type :llx.agent.command/wait}))
  (is (sut/valid? :llx.agent/command
                  {:llx.agent.command/type :llx.agent.command/shutdown}))

  (is (sut/valid? :llx.agent/runtime-turn-value
                  {:status :ok}))
  (is (not (sut/valid? :llx.agent/runtime-turn-value
                       {:status "ok"})))

  (is (sut/valid? :llx.agent/runtime-turn-result
                  {:result  (p/resolved {:status :ok})
                   :cancel! (fn [] nil)}))
  (is (not (sut/valid? :llx.agent/runtime-turn-result
                       {:result (p/resolved {:status :ok})})))

  (is (sut/valid? :llx.agent/runtime-options
                  {:run-command! (fn [_]
                                   {:result  (p/resolved {:status :ok})
                                    :cancel! (fn [] nil)})}))
  (is (sut/valid? :llx.agent/runtime-options
                  {:run-command!  (fn [_]
                                    {:result  (p/resolved {:status :ok})
                                     :cancel! (fn [] nil)})
                   :initial-state {:messages [valid-user-message]}}))
  (is (sut/valid? :llx.agent/runtime-options
                  {:run-command!       (fn [_]
                                         {:result  (p/resolved {:status :ok})
                                          :cancel! (fn [] nil)})
                   :convert-to-llm     (fn [messages] messages)
                   :transform-context  (fn [messages _signal]
                                         (p/resolved messages))
                   :get-api-key        (fn [_provider] "api-key")
                   :stream-fn          (fn [_model _context _opts])
                   :session-id         "session-123"
                   :thinking-budgets   {:minimal 128 :low 512}
                   :max-retry-delay-ms 60000}))
  (is (not (sut/valid? :llx.agent/runtime-options
                       {:steering-mode :all})))
  (is (not (sut/valid? :llx.agent/runtime-options
                       {:run-command!       (fn [_]
                                              {:result  (p/resolved {:status :ok})
                                               :cancel! (fn [] nil)})
                        :max-retry-delay-ms "60000"})))
  (is (not (sut/valid? :llx.agent/runtime-options
                       {:run-command!     (fn [_]
                                            {:result  (p/resolved {:status :ok})
                                             :cancel! (fn [] nil)})
                        :thinking-budgets {:minimal "128"}})))
  (is (not (sut/valid? :llx.agent/runtime-options
                       {:run-command!  (fn [_]
                                         {:result  (p/resolved {:status :ok})
                                          :cancel! (fn [] nil)})
                        :initial-state {:messages [{:role :assistant}]}})))

  (is (not (sut/valid? :llx.agent/command
                       {:llx.agent.command/type :prompt})))
  (is (not (sut/valid? :llx.agent/command
                       {:llx.agent.command/type :llx.agent.command/prompt}))))

(deftest proxy-and-agent-option-schemas
  (is (sut/valid? :llx.agent/proxy-event {:llx.agent.proxy-event/type :start}))
  (is (sut/valid? :llx.agent/proxy-event {:llx.agent.proxy-event/type          :text-start
                                          :llx.agent.proxy-event/content-index 0}))
  (is (sut/valid? :llx.agent/proxy-event {:llx.agent.proxy-event/type          :text-delta
                                          :llx.agent.proxy-event/content-index 0
                                          :llx.agent.proxy-event/delta         "he"}))
  (is (sut/valid? :llx.agent/proxy-event
                  {:llx.agent.proxy-event/type              :text-end
                   :llx.agent.proxy-event/content-index     0
                   :llx.agent.proxy-event/content-signature "sig"}))
  (is (sut/valid? :llx.agent/proxy-event {:llx.agent.proxy-event/type          :thinking-start
                                          :llx.agent.proxy-event/content-index 0}))
  (is (sut/valid? :llx.agent/proxy-event {:llx.agent.proxy-event/type          :thinking-delta
                                          :llx.agent.proxy-event/content-index 0
                                          :llx.agent.proxy-event/delta         "..."}))
  (is (sut/valid? :llx.agent/proxy-event
                  {:llx.agent.proxy-event/type              :thinking-end
                   :llx.agent.proxy-event/content-index     0
                   :llx.agent.proxy-event/content-signature "sig"}))
  (is (sut/valid? :llx.agent/proxy-event
                  {:llx.agent.proxy-event/type          :toolcall-start
                   :llx.agent.proxy-event/content-index 0
                   :llx.agent.proxy-event/id            "call_1"
                   :llx.agent.proxy-event/tool-name     "search"}))
  (is (sut/valid? :llx.agent/proxy-event
                  {:llx.agent.proxy-event/type          :toolcall-delta
                   :llx.agent.proxy-event/content-index 0
                   :llx.agent.proxy-event/delta         "{\"q\":\"x\"}"}))
  (is (sut/valid? :llx.agent/proxy-event {:llx.agent.proxy-event/type          :toolcall-end
                                          :llx.agent.proxy-event/content-index 0}))
  (is (sut/valid? :llx.agent/proxy-event {:llx.agent.proxy-event/type   :done
                                          :llx.agent.proxy-event/reason :stop
                                          :llx.agent.proxy-event/usage  valid-usage}))
  (is (sut/valid? :llx.agent/proxy-event
                  {:llx.agent.proxy-event/type          :error
                   :llx.agent.proxy-event/reason        :error
                   :llx.agent.proxy-event/error-message "boom"
                   :llx.agent.proxy-event/usage         valid-usage}))

  (is (sut/valid? :llx.agent/proxy-options
                  {:proxy-url  "https://agent.example/stream"
                   :auth-token "token"
                   :reasoning  :medium
                   :max-tokens 512
                   :headers    {"x-trace-id" "abc"}}))

  (is (sut/valid? :llx.agent/agent-options
                  {:initial-state  {:system-prompt      "x"
                                    :model              valid-model
                                    :thinking-level     :off
                                    :tools              []
                                    :messages           []
                                    :streaming?         false
                                    :stream-message     nil
                                    :pending-tool-calls #{}
                                    :error              nil}
                   :convert-to-llm (fn [msgs] msgs)
                   :stream-fn      (fn [_model _context _opts])
                   :steering-mode  :one-at-a-time
                   :follow-up-mode :all
                   :max-tokens     512}))
  (is (not (sut/valid? :llx.agent/proxy-options {:auth-token "x"}))))

(deftest fsm-and-fx-schemas
  (let [effect {:op :llx.agent.fsm/fx-start-turn}
        event  {:name :llx.agent.fsm/cmd-prompt :data {:messages [valid-user-message]}}
        plan   {:state              :llx.agent.fsm/idle
                :event              :llx.agent.fsm/cmd-prompt
                :payload            {:messages [valid-user-message]}
                :planned-next-state :llx.agent.fsm/starting
                :effects            [effect]
                :supported?         true}
        step   (assoc plan
                      :before        :llx.agent.fsm/idle
                      :after         :llx.agent.fsm/starting
                      :active-states #{:llx.agent.fsm/runtime :llx.agent.fsm/starting})]
    (is (sut/valid? :llx.agent.fsm/event event))
    (is (sut/valid? :llx.agent.fsm/event-or-name event))
    (is (sut/valid? :llx.agent.fsm/event-or-name :llx.agent.fsm/cmd-reset))
    (is (sut/valid? :llx.agent.fsm/effect effect))
    (is (sut/valid? :llx.agent.fsm/transition-plan plan))
    (is (sut/valid? :llx.agent.fsm/step step))
    (is (sut/valid? :llx.agent.fsm/new-env-options {:session-id :demo
                                                    :system-env {::example true}}))
    (is (sut/valid? :llx.agent.fsm/env {:system-env {} :session-id :demo :chart-src :chart}))
    (is (not (sut/valid? :llx.agent.fsm/env {:system-env {}})))

    (is (sut/valid? :llx.agent.fx/callbacks {:start-turn! (fn [_ _] :ok)}))
    (is (sut/valid? :llx.agent.fx/handlers {:llx.agent.fsm/fx-start-turn (fn [_ _] :ok)}))
    (is (sut/valid? :llx.agent.fx/context {:handlers           {:llx.agent.fsm/fx-start-turn (fn [_ _] :ok)}
                                           :on-missing-handler :ignore}))
    (is (sut/valid? :llx.agent.fx/effects-options {:continue-on-error? true}))
    (is (sut/valid? :llx.agent.fx/effect-result {:effect effect :status :ok :value :ok}))
    (is (sut/valid? :llx.agent.fx/step-result {:step step :effect-results [{:effect effect :status :ok}]}))
    (is (not (sut/valid? :llx.agent.fx/effect-result {:status :ok})))))

(deftest registry-contains-schema-ids
  (doseq [schema-id [:llx.agent/thinking-level
                     :llx.agent/tool-result
                     :llx.agent/tool
                     :llx.agent/context
                     :llx.agent/message
                     :llx.agent/state
                     :llx.agent/loop-config
                     :llx.agent/stream-fn
                     :llx.agent/event-agent-start
                     :llx.agent/event-agent-end
                     :llx.agent/event-turn-start
                     :llx.agent/event-turn-end
                     :llx.agent/event-message-start
                     :llx.agent/event-message-update
                     :llx.agent/event-message-end
                     :llx.agent/event-tool-execution-start
                     :llx.agent/event-tool-execution-update
                     :llx.agent/event-tool-execution-end
                     :llx.agent/event
                     :llx.agent/command-type
                     :llx.agent/command
                     :llx.agent/runtime-turn-value
                     :llx.agent/runtime-turn-result
                     :llx.agent/runtime-initial-state
                     :llx.agent/runtime-options
                     :llx.agent.fsm/state-id
                     :llx.agent.fsm/event-id
                     :llx.agent.fsm/effect-op
                     :llx.agent.fsm/event
                     :llx.agent.fsm/event-or-name
                     :llx.agent.fsm/effect
                     :llx.agent.fsm/effects
                     :llx.agent.fsm/transition-plan
                     :llx.agent.fsm/step
                     :llx.agent.fsm/new-env-options
                     :llx.agent.fsm/env
                     :llx.agent.fx/callback-key
                     :llx.agent.fx/callbacks
                     :llx.agent.fx/handlers
                     :llx.agent.fx/context
                     :llx.agent.fx/effects-options
                     :llx.agent.fx/effect-result
                     :llx.agent.fx/step-result
                     :llx.agent/proxy-event
                     :llx.agent/proxy-options
                     :llx.agent/agent-options]]
    (is (contains? (sut/registry) schema-id) (str "missing " schema-id))))
