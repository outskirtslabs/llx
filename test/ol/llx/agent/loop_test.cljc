(ns ol.llx.agent.loop-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [deftest is testing]]])
   [ol.llx.ai :as ai]
   [ol.llx.agent.fx :as fx]
   [ol.llx.agent.loop :as sut]))

(defn- initial-state
  []
  {:ol.llx.agent.loop/phase :ol.llx.agent.loop/idle
   :system-prompt           ""
   :model                   (ai/get-model :openai "gpt-5.2-codex")
   :thinking-level          :off
   :tools                   []
   :messages                []
   :stream-message          nil
   :pending-tool-calls      []
   :error                   nil
   :steering-queue          sut/empty-queue
   :follow-up-queue         sut/empty-queue
   :steering-mode           :one-at-a-time
   :follow-up-mode          :one-at-a-time})

(defn- state-with
  [overrides]
  (merge (initial-state) overrides))

(defn- fx-types
  [effects]
  (mapv ::fx/type effects))

(defn- tool-call-block
  [id name arguments]
  {:type :tool-call :id id :name name :arguments arguments})

(defn- tool-result-message
  [tool-call-id tool-name text is-error?]
  {:role         :tool-result
   :tool-call-id tool-call-id
   :tool-name    tool-name
   :content      [{:type :text :text text}]
   :is-error?    is-error?
   :timestamp    1})

(deftest initial-state-has-expected-shape-test
  (is (= {:ol.llx.agent.loop/phase :ol.llx.agent.loop/idle
          :system-prompt           ""
          :model                   (ai/get-model :openai "gpt-5.2-codex")
          :thinking-level          :off
          :tools                   []
          :messages                []
          :stream-message          nil
          :pending-tool-calls      []
          :error                   nil
          :steering-queue          sut/empty-queue
          :follow-up-queue         sut/empty-queue
          :steering-mode           :one-at-a-time
          :follow-up-mode          :one-at-a-time}
         (initial-state))))

(deftest command-predicate-test
  (testing "recognizes commands"
    (is (true? (sut/command? {:type :ol.llx.agent.command/prompt})))
    (is (true? (sut/command? {:type :ol.llx.agent.command/abort})))
    (is (true? (sut/command? {:type :ol.llx.agent.command/steer}))))

  (testing "rejects signals"
    (is (false? (sut/command? {:type :ol.llx.agent.signal/llm-done})))
    (is (false? (sut/command? {:type :ol.llx.agent.signal/prompt-start}))))

  (testing "rejects events"
    (is (false? (sut/command? {:type :ol.llx.agent.event/agent-start})))))

(deftest handle-command-set-system-prompt-test
  (let [[state' sigs] (sut/handle-command (initial-state)
                                          {:type          :ol.llx.agent.command/set-system-prompt
                                           :system-prompt "You are helpful."})]
    (is (= "You are helpful." (:system-prompt state')))
    (is (= [] sigs))))

(deftest handle-command-set-model-test
  (let [[state' sigs] (sut/handle-command (initial-state)
                                          {:type  :ol.llx.agent.command/set-model
                                           :model {:id "gpt-4o"}})]
    (is (= {:id "gpt-4o"} (:model state')))
    (is (= [] sigs))))

(deftest handle-command-set-thinking-level-test
  (let [[state' sigs] (sut/handle-command (initial-state)
                                          {:type           :ol.llx.agent.command/set-thinking-level
                                           :thinking-level :high})]
    (is (= :high (:thinking-level state')))
    (is (= [] sigs))))

(deftest handle-command-set-tools-test
  (let [tools         [{:name "read_file"}]
        [state' sigs] (sut/handle-command (initial-state)
                                          {:type  :ol.llx.agent.command/set-tools
                                           :tools tools})]
    (is (= tools (:tools state')))
    (is (= [] sigs))))

(deftest handle-command-set-steering-mode-test
  (let [[state' sigs] (sut/handle-command (initial-state)
                                          {:type :ol.llx.agent.command/set-steering-mode
                                           :mode :all})]
    (is (= :all (:steering-mode state')))
    (is (= [] sigs))))

(deftest handle-command-set-follow-up-mode-test
  (let [[state' sigs] (sut/handle-command (initial-state)
                                          {:type :ol.llx.agent.command/set-follow-up-mode
                                           :mode :all})]
    (is (= :all (:follow-up-mode state')))
    (is (= [] sigs))))

(deftest handle-command-replace-messages-test
  (let [msgs          [{:role :user :content "a"} {:role :assistant :content "b"}]
        [state' sigs] (sut/handle-command (state-with {:messages [{:role :user :content "old"}]})
                                          {:type     :ol.llx.agent.command/replace-messages
                                           :messages msgs})]
    (is (= msgs (:messages state')))
    (is (= [] sigs))))

(deftest handle-command-append-message-test
  (let [existing      [{:role :user :content "first"}]
        new-msg       {:role :assistant :content "second"}
        [state' sigs] (sut/handle-command (state-with {:messages existing})
                                          {:type    :ol.llx.agent.command/append-message
                                           :message new-msg})]
    (is (= [{:role :user :content "first"}
            {:role :assistant :content "second"}]
           (:messages state')))
    (is (= [] sigs))))

(deftest handle-command-clear-messages-test
  (let [[state' sigs] (sut/handle-command (state-with {:messages [{:role :user :content "x"}]})
                                          {:type :ol.llx.agent.command/clear-messages})]
    (is (= [] (:messages state')))
    (is (= [] sigs))))

(deftest handle-command-steer-test
  (let [msg           {:role :user :content "steer me"}
        [state' sigs] (sut/handle-command (initial-state)
                                          {:type :ol.llx.agent.command/steer :message msg})]
    (is (= [msg] (vec (:steering-queue state'))))
    (is (= [] sigs))))

(deftest handle-command-follow-up-test
  (let [msg           {:role :user :content "follow up"}
        [state' sigs] (sut/handle-command (initial-state)
                                          {:type :ol.llx.agent.command/follow-up :message msg})]
    (is (= [msg] (vec (:follow-up-queue state'))))
    (is (= [] sigs))))

(deftest handle-command-clear-steering-queue-test
  (let [state         (-> (initial-state)
                          (update :steering-queue conj {:role :user :content "a"}))
        [state' sigs] (sut/handle-command state {:type :ol.llx.agent.command/clear-steering-queue})]
    (is (empty? (:steering-queue state')))
    (is (= [] sigs))))

(deftest handle-command-clear-follow-up-queue-test
  (let [state         (-> (initial-state)
                          (update :follow-up-queue conj {:role :user :content "a"}))
        [state' sigs] (sut/handle-command state {:type :ol.llx.agent.command/clear-follow-up-queue})]
    (is (empty? (:follow-up-queue state')))
    (is (= [] sigs))))

(deftest handle-command-clear-all-queues-test
  (let [state         (-> (initial-state)
                          (update :steering-queue conj {:role :user :content "s"})
                          (update :follow-up-queue conj {:role :user :content "f"}))
        [state' sigs] (sut/handle-command state {:type :ol.llx.agent.command/clear-all-queues})]
    (is (empty? (:steering-queue state')))
    (is (empty? (:follow-up-queue state')))
    (is (= [] sigs))))

(deftest handle-command-reset-test
  (let [state         (-> (initial-state)
                          (assoc :ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                 :messages [{:role :user :content "x"}]
                                 :stream-message {:partial true}
                                 :pending-tool-calls ["tc-1"]
                                 :error "boom")
                          (update :steering-queue conj {:role :user :content "s"})
                          (update :follow-up-queue conj {:role :user :content "f"}))
        [state' sigs] (sut/handle-command state {:type :ol.llx.agent.command/reset})]
    (is (= {:ol.llx.agent.loop/phase :ol.llx.agent.loop/idle
            :messages                []
            :steering-queue          sut/empty-queue
            :follow-up-queue         sut/empty-queue
            :stream-message          nil
            :pending-tool-calls      []
            :error                   nil}
           (select-keys state' [:ol.llx.agent.loop/phase :messages :steering-queue :follow-up-queue
                                :stream-message :pending-tool-calls :error])))
    (is (= [] sigs))))

(deftest handle-command-prompt-from-idle-test
  (let [msgs          [{:role :user :content "hello"}]
        [state' sigs] (sut/handle-command (initial-state)
                                          {:type :ol.llx.agent.command/prompt :messages msgs})]
    (is (= (initial-state) state'))
    (is (= [{:type :ol.llx.agent.signal/prompt-start :messages msgs}] sigs))))

(deftest handle-command-prompt-when-not-idle-test
  (let [state         (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming})
        [state' sigs] (sut/handle-command state
                                          {:type     :ol.llx.agent.command/prompt
                                           :messages [{:role :user :content "hi"}]})]
    (is (= state state'))
    (is (= [{:type :ol.llx.agent.signal/rejected :reason :not-idle}] sigs))))

(deftest handle-command-continue-with-steering-one-at-a-time-test
  (let [state         (-> (initial-state)
                          (update :steering-queue conj {:role :user :content "s1"})
                          (update :steering-queue conj {:role :user :content "s2"}))
        [state' sigs] (sut/handle-command state {:type :ol.llx.agent.command/continue})]
    (is (= [{:role :user :content "s1"}]
           (:messages (first sigs))))
    (is (= :ol.llx.agent.signal/continue-start (:type (first sigs))))
    (is (= [{:role :user :content "s2"}]
           (vec (:steering-queue state'))))))

(deftest handle-command-continue-with-steering-all-mode-test
  (let [state         (-> (initial-state)
                          (assoc :steering-mode :all)
                          (update :steering-queue conj {:role :user :content "s1"})
                          (update :steering-queue conj {:role :user :content "s2"}))
        [state' sigs] (sut/handle-command state {:type :ol.llx.agent.command/continue})]
    (is (= [{:role :user :content "s1"} {:role :user :content "s2"}]
           (:messages (first sigs))))
    (is (empty? (:steering-queue state')))))

(deftest handle-command-continue-with-follow-up-one-at-a-time-test
  (let [state         (-> (initial-state)
                          (update :follow-up-queue conj {:role :user :content "f1"})
                          (update :follow-up-queue conj {:role :user :content "f2"}))
        [state' sigs] (sut/handle-command state {:type :ol.llx.agent.command/continue})]
    (is (= :ol.llx.agent.signal/continue-start (:type (first sigs))))
    (is (= [{:role :user :content "f1"}]
           (:messages (first sigs))))
    (is (= [{:role :user :content "f2"}]
           (vec (:follow-up-queue state'))))))

(deftest handle-command-continue-with-follow-up-all-mode-test
  (let [state         (-> (initial-state)
                          (assoc :follow-up-mode :all)
                          (update :follow-up-queue conj {:role :user :content "f1"})
                          (update :follow-up-queue conj {:role :user :content "f2"}))
        [state' sigs] (sut/handle-command state {:type :ol.llx.agent.command/continue})]
    (is (= [{:role :user :content "f1"} {:role :user :content "f2"}]
           (:messages (first sigs))))
    (is (empty? (:follow-up-queue state')))))

(deftest handle-command-continue-steering-before-follow-up-test
  (let [state         (-> (initial-state)
                          (update :steering-queue conj {:role :user :content "steer"})
                          (update :follow-up-queue conj {:role :user :content "follow"}))
        [state' sigs] (sut/handle-command state {:type :ol.llx.agent.command/continue})]
    (is (= [{:role :user :content "steer"}]
           (:messages (first sigs))))
    (is (= [{:role :user :content "follow"}]
           (vec (:follow-up-queue state'))))))

(deftest handle-command-continue-empty-queues-test
  (let [[state' sigs] (sut/handle-command (initial-state)
                                          {:type :ol.llx.agent.command/continue})]
    (is (= (initial-state) state'))
    (is (= [{:type :ol.llx.agent.signal/rejected :reason :no-queued-messages}] sigs))))

(deftest handle-command-continue-when-not-idle-test
  (let [state         (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming})
        [state' sigs] (sut/handle-command state {:type :ol.llx.agent.command/continue})]
    (is (= state state'))
    (is (= [{:type :ol.llx.agent.signal/rejected :reason :not-idle}] sigs))))

(deftest handle-command-abort-when-streaming-test
  (let [state         (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming})
        [state' sigs] (sut/handle-command state {:type :ol.llx.agent.command/abort})]
    (is (= state state'))
    (is (= [{:type :ol.llx.agent.signal/abort}] sigs))))

(deftest handle-command-abort-when-idle-test
  (let [[state' sigs] (sut/handle-command (initial-state)
                                          {:type :ol.llx.agent.command/abort})]
    (is (= (initial-state) state'))
    (is (= [{:type :ol.llx.agent.signal/rejected :reason :idle}] sigs))))

(deftest idle-transition-prompt-start-test
  (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/idle :messages [{:role :user :content "old"}]})
        new-msgs         [{:role :user :content "hello"}]
        [state' effects] (sut/idle-transition state {:type     :ol.llx.agent.signal/prompt-start
                                                     :messages new-msgs})]
    (testing "appends messages and transitions to streaming"
      (is (= [{:role :user :content "old"} {:role :user :content "hello"}]
             (:messages state')))
      (is (= :ol.llx.agent.loop/streaming (:ol.llx.agent.loop/phase state'))))

    (testing "emits agent-start, turn-start, message events, and call-llm"
      (is (= [:emit-event :emit-event :emit-event :emit-event :call-llm]
             (fx-types effects)))
      (is (= :ol.llx.agent.event/agent-start (get-in effects [0 :event :type])))
      (is (= :ol.llx.agent.event/turn-start (get-in effects [1 :event :type])))
      (is (= :ol.llx.agent.event/message-start (get-in effects [2 :event :type])))
      (is (= :ol.llx.agent.event/message-end (get-in effects [3 :event :type]))))

    (testing "call-llm receives the full message history including new messages"
      (let [call-llm-fx (last effects)]
        (is (= [{:role :user :content "old"} {:role :user :content "hello"}]
               (:messages call-llm-fx)))))))

(deftest idle-transition-continue-start-test
  (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/idle :messages [{:role :user :content "old"}]})
        new-msgs         [{:role :user :content "continue"}]
        [state' effects] (sut/idle-transition state {:type     :ol.llx.agent.signal/continue-start
                                                     :messages new-msgs})]
    (testing "appends messages and transitions to streaming"
      (is (= [{:role :user :content "old"} {:role :user :content "continue"}]
             (:messages state')))
      (is (= :ol.llx.agent.loop/streaming (:ol.llx.agent.loop/phase state'))))

    (testing "emits turn-start (no agent-start), message events, and call-llm"
      (is (= [:emit-event :emit-event :emit-event :call-llm]
             (fx-types effects)))
      (is (= :ol.llx.agent.event/turn-start (get-in effects [0 :event :type]))))

    (testing "call-llm receives the full message history including new messages"
      (let [call-llm-fx (last effects)]
        (is (= [{:role :user :content "old"} {:role :user :content "continue"}]
               (:messages call-llm-fx)))))))

(deftest idle-transition-invalid-signal-test
  (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/idle})
        [state' effects] (sut/idle-transition state {:type :ol.llx.agent.signal/llm-chunk})]
    (is (= state state'))
    (is (= [{::fx/type :reject :reason :invalid-signal}] effects))))

(deftest streaming-transition-llm-start-test
  (let [msg              {:role :assistant :content "partial"}
        state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming})
        [state' effects] (sut/streaming-transition state {:type    :ol.llx.agent.signal/llm-start
                                                          :message msg})]
    (is (= msg (:stream-message state')))
    (is (= [{::fx/type :emit-event
             :event    {:type :ol.llx.agent.event/message-start :message msg}}]
           effects))))

(deftest streaming-transition-llm-chunk-test
  (let [chunk            {:role :assistant :content "more"}
        state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                      :stream-message          {:role :assistant :content "partial"}})
        [state' effects] (sut/streaming-transition state {:type  :ol.llx.agent.signal/llm-chunk
                                                          :chunk chunk})]
    (is (= chunk (:stream-message state')))
    (is (= [{::fx/type :emit-event
             :event    {:type :ol.llx.agent.event/message-update :chunk chunk}}]
           effects))))

(deftest streaming-transition-llm-done-no-tools-test
  (let [message          {:role :assistant :content [{:type :text :text "Hi!"}]}
        state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                      :messages                [{:role :user :content "hello"}]
                                      :stream-message          {:role :assistant :content "partial"}})
        [state' effects] (sut/streaming-transition state {:type    :ol.llx.agent.signal/llm-done
                                                          :message message})]
    (testing "transitions to idle and clears stream-message"
      (is (= :ol.llx.agent.loop/idle (:ol.llx.agent.loop/phase state')))
      (is (nil? (:stream-message state'))))

    (testing "appends the final message"
      (is (= [{:role :user :content "hello"} message]
             (:messages state'))))

    (testing "emits message-end and turn-end"
      (is (= [:emit-event :emit-event] (fx-types effects)))
      (is (= :ol.llx.agent.event/message-end (get-in effects [0 :event :type])))
      (is (= :ol.llx.agent.event/turn-end (get-in effects [1 :event :type]))))))

(deftest streaming-transition-llm-done-with-steering-queue-starts-next-turn-test
  (let [message          {:role :assistant :content [{:type :text :text "Done"}]}
        steering-message {:role :user :content "next steer"}
        state            (-> (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                          :messages                [{:role :user :content "hello"}]
                                          :stream-message          {:role :assistant :content "partial"}})
                             (update :steering-queue conj steering-message))
        [state' effects] (sut/streaming-transition state {:type    :ol.llx.agent.signal/llm-done
                                                          :message message})]
    (testing "moves directly into the next streaming turn"
      (is (= :ol.llx.agent.loop/streaming (:ol.llx.agent.loop/phase state')))
      (is (nil? (:stream-message state')))
      (is (empty? (:steering-queue state'))))

    (testing "appends the finished assistant message and queued steering message"
      (is (= [{:role :user :content "hello"} message steering-message]
             (:messages state'))))

    (testing "continues the run by ending the turn and scheduling more llm work"
      (is (= :call-llm (::fx/type (last effects))))
      (is (= (:messages state') (:messages (last effects))))
      (is (some #(= :ol.llx.agent.event/turn-end (get-in % [:event :type])) effects))
      (is (some #(= :ol.llx.agent.event/turn-start (get-in % [:event :type])) effects)))))

(deftest streaming-transition-llm-done-with-follow-up-queue-starts-next-turn-test
  (let [message          {:role :assistant :content [{:type :text :text "Done"}]}
        follow-up        {:role :user :content "queued follow-up"}
        state            (-> (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                          :messages                [{:role :user :content "hello"}]
                                          :stream-message          {:role :assistant :content "partial"}})
                             (update :follow-up-queue conj follow-up))
        [state' effects] (sut/streaming-transition state {:type    :ol.llx.agent.signal/llm-done
                                                          :message message})]
    (testing "starts a new turn from follow-up messages when no steering exists"
      (is (= :ol.llx.agent.loop/streaming (:ol.llx.agent.loop/phase state')))
      (is (empty? (:follow-up-queue state')))
      (is (= [{:role :user :content "hello"} message follow-up]
             (:messages state'))))

    (testing "continues the run with another llm call"
      (is (= :call-llm (::fx/type (last effects))))
      (is (= (:messages state') (:messages (last effects))))
      (is (some #(= :ol.llx.agent.event/turn-start (get-in % [:event :type])) effects)))))

(deftest streaming-transition-llm-done-prefers-steering-over-follow-up-test
  (let [message          {:role :assistant :content [{:type :text :text "Done"}]}
        steering-message {:role :user :content "steer first"}
        follow-up        {:role :user :content "follow later"}
        state            (-> (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                          :messages                [{:role :user :content "hello"}]
                                          :stream-message          {:role :assistant :content "partial"}})
                             (update :steering-queue conj steering-message)
                             (update :follow-up-queue conj follow-up))
        [state' effects] (sut/streaming-transition state {:type    :ol.llx.agent.signal/llm-done
                                                          :message message})]
    (testing "uses steering messages first and leaves follow-up queued"
      (is (= :ol.llx.agent.loop/streaming (:ol.llx.agent.loop/phase state')))
      (is (empty? (:steering-queue state')))
      (is (= [follow-up] (vec (:follow-up-queue state'))))
      (is (= [{:role :user :content "hello"} message steering-message]
             (:messages state'))))

    (testing "continues immediately with call-llm"
      (is (= :call-llm (::fx/type (last effects))))
      (is (= (:messages state') (:messages (last effects)))))))

(deftest streaming-transition-llm-done-with-tools-test
  (let [tool-calls       [(tool-call-block "tc-1" "read_file" {:path "/tmp"})
                          (tool-call-block "tc-2" "write_file" {:path "/out"})]
        message          {:role    :assistant
                          :content [{:type :text :text "Working..."}
                                    (first tool-calls)
                                    (second tool-calls)]}
        state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                      :messages                [{:role :user :content "do stuff"}]
                                      :stream-message          {:role :assistant :content "partial"}})
        [state' effects] (sut/streaming-transition state {:type    :ol.llx.agent.signal/llm-done
                                                          :message message})]
    (testing "transitions to tool-executing"
      (is (= :ol.llx.agent.loop/tool-executing (:ol.llx.agent.loop/phase state'))))

    (testing "sets pending-tool-calls and clears stream-message"
      (is (= [{:id "tc-1" :name "read_file" :arguments {:path "/tmp"}}
              {:id "tc-2" :name "write_file" :arguments {:path "/out"}}]
             (:pending-tool-calls state')))
      (is (nil? (:stream-message state'))))

    (testing "appends the message"
      (is (= [{:role :user :content "do stuff"} message]
             (:messages state'))))

    (testing "emits message-end, tool-execution-start, and execute-tool"
      (is (= [:emit-event :emit-event :execute-tool] (fx-types effects)))
      (is (= :ol.llx.agent.event/message-end (get-in effects [0 :event :type])))
      (is (= :ol.llx.agent.event/tool-execution-start (get-in effects [1 :event :type])))
      (is (= "tc-1" (get-in effects [1 :event :tool-call-id])))
      (is (= {:id "tc-1" :name "read_file" :arguments {:path "/tmp"}}
             (:tool-call (nth effects 2)))))))

(deftest streaming-transition-llm-error-test
  (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                      :messages                [{:role :user :content "hello"}]})
        [state' effects] (sut/streaming-transition state {:type  :ol.llx.agent.signal/llm-error
                                                          :error "connection reset"})]
    (testing "transitions to idle and sets error"
      (is (= :ol.llx.agent.loop/idle (:ol.llx.agent.loop/phase state')))
      (is (= "connection reset" (:error state'))))

    (testing "emits message-end and turn-end"
      (is (= [:emit-event :emit-event] (fx-types effects)))
      (is (= :ol.llx.agent.event/message-end (get-in effects [0 :event :type])))
      (is (= :ol.llx.agent.event/turn-end (get-in effects [1 :event :type]))))))

(deftest streaming-transition-abort-test
  (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                      :messages                [{:role :user :content "hello"}]
                                      :stream-message          {:role :assistant :content "partial"}})
        [state' effects] (sut/streaming-transition state {:type :ol.llx.agent.signal/abort})]
    (testing "transitions to idle and clears stream-message"
      (is (= :ol.llx.agent.loop/idle (:ol.llx.agent.loop/phase state')))
      (is (nil? (:stream-message state'))))

    (testing "emits message-end and turn-end"
      (is (= [:emit-event :emit-event] (fx-types effects)))
      (is (= :ol.llx.agent.event/message-end (get-in effects [0 :event :type])))
      (is (= :ol.llx.agent.event/turn-end (get-in effects [1 :event :type]))))))

(deftest streaming-transition-unknown-signal-test
  (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming})
        [state' effects] (sut/streaming-transition state {:type :ol.llx.agent.signal/tool-result})]
    (is (= state state'))
    (is (= [] effects))))

(deftest tool-executing-transition-tool-result-with-remaining-test
  (let [tool-calls       [{:id "tc-1" :name "read_file" :arguments {:path "/a"}}
                          {:id "tc-2" :name "write_file" :arguments {:path "/b"}}]
        result           (tool-result-message "tc-1" "read_file" "file contents" false)
        tool-result-msg  result
        state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/tool-executing
                                      :pending-tool-calls      tool-calls
                                      :messages                [{:role :user :content "do stuff"}]})
        [state' effects] (sut/tool-executing-transition
                          state
                          {:type                :ol.llx.agent.signal/tool-result
                           :result              result
                           :tool-result-message tool-result-msg})]
    (testing "processes result and keeps remaining tools"
      (is (= [{:id "tc-2" :name "write_file" :arguments {:path "/b"}}]
             (:pending-tool-calls state'))))

    (testing "appends result to messages"
      (is (= [{:role :user :content "do stuff"} result]
             (:messages state'))))

    (testing "emits tool-execution-end, message events, and starts next tool"
      (is (= [:emit-event :emit-event :emit-event :emit-event :execute-tool]
             (fx-types effects)))
      (is (= :ol.llx.agent.event/tool-execution-end (get-in effects [0 :event :type])))
      (is (= "tc-1" (get-in effects [0 :event :tool-call-id])))
      (is (= "read_file" (get-in effects [0 :event :tool-name])))
      (is (= result (get-in effects [0 :event :result])))
      (is (false? (get-in effects [0 :event :is-error?])))
      (is (= :ol.llx.agent.event/message-start (get-in effects [1 :event :type])))
      (is (= :ol.llx.agent.event/message-end (get-in effects [2 :event :type])))
      (is (= :ol.llx.agent.event/tool-execution-start (get-in effects [3 :event :type])))
      (is (= "tc-2" (get-in effects [3 :event :tool-call-id]))))))

(deftest tool-executing-transition-tool-result-with-steering-interrupts-remaining-tools-test
  (let [tool-calls       [{:id "tc-1" :name "read_file" :arguments {:path "/a"}}
                          {:id "tc-2" :name "write_file" :arguments {:path "/b"}}
                          {:id "tc-3" :name "echo" :arguments {:text "c"}}]
        steering-msg-1   {:role :user :content "interrupt-1"}
        steering-msg-2   {:role :user :content "interrupt-2"}
        result           (tool-result-message "tc-1" "read_file" "file contents" false)
        state            (-> (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/tool-executing
                                          :pending-tool-calls      tool-calls
                                          :messages                [{:role :user :content "do stuff"}]})
                             (update :steering-queue conj steering-msg-1)
                             (update :steering-queue conj steering-msg-2))
        [state' effects] (sut/tool-executing-transition
                          state
                          {:type                :ol.llx.agent.signal/tool-result
                           :result              result
                           :tool-result-message result})]
    (testing "clears pending tool calls and keeps remaining steering queued in one-at-a-time mode"
      (is (= [] (:pending-tool-calls state')))
      (is (= [steering-msg-2] (vec (:steering-queue state')))))

    (testing "appends current tool result, synthetic skipped results, and dequeued steering message"
      (is (= [{:role :user :content "do stuff"}
              (dissoc result :timestamp)
              {:role         :tool-result
               :tool-call-id "tc-2"
               :tool-name    "write_file"
               :content      [{:type :text :text "Skipped due to queued user message."}]
               :is-error?    true}
              {:role         :tool-result
               :tool-call-id "tc-3"
               :tool-name    "echo"
               :content      [{:type :text :text "Skipped due to queued user message."}]
               :is-error?    true}
              steering-msg-1]
             (mapv #(dissoc % :timestamp) (:messages state')))))

    (testing "emits current result events, skipped tool lifecycle events, steering message events, and resumes llm"
      (is (= [:emit-event :emit-event :emit-event
              :emit-event :emit-event :emit-event :emit-event
              :emit-event :emit-event :emit-event :emit-event
              :emit-event :emit-event
              :call-llm]
             (fx-types effects)))
      (is (= :ol.llx.agent.event/tool-execution-end (get-in effects [0 :event :type])))
      (is (= "tc-2" (get-in effects [3 :event :tool-call-id])))
      (is (= true (get-in effects [4 :event :is-error?])))
      (is (= "tc-3" (get-in effects [7 :event :tool-call-id])))
      (is (= true (get-in effects [8 :event :is-error?])))
      (is (= :ol.llx.agent.event/message-start (get-in effects [11 :event :type])))
      (is (= steering-msg-1 (get-in effects [11 :event :message])))
      (is (= :ol.llx.agent.event/message-end (get-in effects [12 :event :type])))
      (is (= (:messages state') (:messages (last effects)))))))

(deftest tool-executing-transition-tool-result-with-steering-all-mode-dequeues-all-test
  (let [tool-calls        [{:id "tc-1" :name "read_file" :arguments {:path "/a"}}
                           {:id "tc-2" :name "write_file" :arguments {:path "/b"}}]
        steering-msg-1    {:role :user :content "interrupt-1"}
        steering-msg-2    {:role :user :content "interrupt-2"}
        result            (tool-result-message "tc-1" "read_file" "file contents" false)
        state             (-> (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/tool-executing
                                           :pending-tool-calls      tool-calls
                                           :messages                [{:role :user :content "do stuff"}]
                                           :steering-mode           :all})
                              (update :steering-queue conj steering-msg-1)
                              (update :steering-queue conj steering-msg-2))
        [state' _effects] (sut/tool-executing-transition
                           state
                           {:type                :ol.llx.agent.signal/tool-result
                            :result              result
                            :tool-result-message result})]
    (testing "dequeues all steering messages in :all mode"
      (is (empty? (:steering-queue state')))
      (is (= [steering-msg-1 steering-msg-2]
             (take-last 2 (:messages state')))))))

(deftest tool-executing-transition-tool-result-last-tool-test
  (let [tool-calls       [{:id "tc-1" :name "echo" :arguments {:text "hi"}}]
        result           (tool-result-message "tc-1" "echo" "echoed" false)
        tool-result-msg  result
        state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/tool-executing
                                      :pending-tool-calls      tool-calls
                                      :messages                []})
        [state' effects] (sut/tool-executing-transition
                          state
                          {:type                :ol.llx.agent.signal/tool-result
                           :result              result
                           :tool-result-message tool-result-msg})]
    (testing "clears pending-tool-calls"
      (is (= [] (:pending-tool-calls state'))))

    (testing "appends result to messages and resumes llm"
      (is (= [result] (:messages state')))
      (is (= [:emit-event :emit-event :emit-event :call-llm]
             (fx-types effects)))
      (is (= :ol.llx.agent.event/tool-execution-end (get-in effects [0 :event :type])))
      (is (= :ol.llx.agent.event/message-start (get-in effects [1 :event :type])))
      (is (= :ol.llx.agent.event/message-end (get-in effects [2 :event :type])))
      (is (= [result] (:messages (nth effects 3)))))))

(deftest tool-executing-transition-tool-error-with-remaining-test
  (let [tool-calls       [{:id "tc-1" :name "bad_tool" :arguments {}}
                          {:id "tc-2" :name "good_tool" :arguments {}}]
        tool-result-msg  (tool-result-message "tc-1" "bad_tool" "kaboom" true)
        state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/tool-executing
                                      :pending-tool-calls      tool-calls
                                      :messages                []})
        [state' effects] (sut/tool-executing-transition
                          state
                          {:type                :ol.llx.agent.signal/tool-error
                           :tool-call-id        "tc-1"
                           :error               "kaboom"
                           :tool-result-message tool-result-msg})]
    (testing "records error result and keeps remaining tools"
      (is (= [{:id "tc-2" :name "good_tool" :arguments {}}]
             (:pending-tool-calls state')))
      (is (= [tool-result-msg]
             (:messages state'))))

    (testing "emits tool-execution-end, message events, and starts next tool"
      (is (= [:emit-event :emit-event :emit-event :emit-event :execute-tool]
             (fx-types effects)))
      (is (= true (get-in effects [0 :event :is-error?])))
      (is (= :ol.llx.agent.event/tool-execution-start (get-in effects [3 :event :type])))
      (is (= "tc-2" (get-in effects [3 :event :tool-call-id]))))))

(deftest tool-executing-transition-tool-error-with-steering-interrupts-remaining-tools-test
  (let [tool-calls       [{:id "tc-1" :name "bad_tool" :arguments {}}
                          {:id "tc-2" :name "good_tool" :arguments {}}
                          {:id "tc-3" :name "echo" :arguments {:text "c"}}]
        tool-result-msg  (tool-result-message "tc-1" "bad_tool" "kaboom" true)
        steering-msg-1   {:role :user :content "interrupt-1"}
        steering-msg-2   {:role :user :content "interrupt-2"}
        state            (-> (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/tool-executing
                                          :pending-tool-calls      tool-calls
                                          :messages                [{:role :user :content "do stuff"}]})
                             (update :steering-queue conj steering-msg-1)
                             (update :steering-queue conj steering-msg-2))
        [state' effects] (sut/tool-executing-transition
                          state
                          {:type                :ol.llx.agent.signal/tool-error
                           :tool-call-id        "tc-1"
                           :error               "kaboom"
                           :tool-result-message tool-result-msg})]
    (testing "does not execute the next real tool and resumes llm after synthetic skipped results"
      (is (= [] (:pending-tool-calls state')))
      (is (not-any? #(= :execute-tool (::fx/type %)) effects))
      (is (= :call-llm (::fx/type (last effects)))))

    (testing "keeps one steering message queued in one-at-a-time mode"
      (is (= [steering-msg-2] (vec (:steering-queue state')))))

    (testing "appends error result, skipped tool results, and dequeued steering message"
      (is (= [(dissoc tool-result-msg :timestamp)
              {:role         :tool-result
               :tool-call-id "tc-2"
               :tool-name    "good_tool"
               :content      [{:type :text :text "Skipped due to queued user message."}]
               :is-error?    true}
              {:role         :tool-result
               :tool-call-id "tc-3"
               :tool-name    "echo"
               :content      [{:type :text :text "Skipped due to queued user message."}]
               :is-error?    true}
              steering-msg-1]
             (mapv #(dissoc % :timestamp) (rest (:messages state'))))))))

(deftest tool-executing-transition-tool-error-last-tool-test
  (let [tool-calls       [{:id "tc-1" :name "bad_tool" :arguments {}}]
        tool-result-msg  (tool-result-message "tc-1" "bad_tool" "kaboom" true)
        state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/tool-executing
                                      :pending-tool-calls      tool-calls
                                      :messages                []})
        [state' effects] (sut/tool-executing-transition
                          state
                          {:type                :ol.llx.agent.signal/tool-error
                           :tool-call-id        "tc-1"
                           :error               "kaboom"
                           :tool-result-message tool-result-msg})]
    (testing "clears pending-tool-calls"
      (is (= [] (:pending-tool-calls state'))))

    (testing "records error result and resumes llm"
      (is (= [tool-result-msg] (:messages state')))
      (is (= [:emit-event :emit-event :emit-event :call-llm]
             (fx-types effects)))
      (is (= [tool-result-msg] (:messages (nth effects 3)))))))

(deftest tool-executing-transition-tool-update-test
  (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/tool-executing
                                      :pending-tool-calls      [{:id "tc-1" :name "slow_tool"}]})
        [state' effects] (sut/tool-executing-transition
                          state
                          {:type           :ol.llx.agent.signal/tool-update
                           :tool-call-id   "tc-1"
                           :tool-name      "slow_tool"
                           :partial-result {:progress 50}})]
    (is (= state state'))
    (is (= [{::fx/type :emit-event
             :event    {:type           :ol.llx.agent.event/tool-execution-update
                        :tool-call-id   "tc-1"
                        :tool-name      "slow_tool"
                        :partial-result {:progress 50}}}]
           effects))))

(deftest tool-executing-transition-abort-test
  (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/tool-executing
                                      :pending-tool-calls      [{:id "tc-1"} {:id "tc-2"}]
                                      :messages                [{:role :user :content "x"}]})
        [state' effects] (sut/tool-executing-transition state {:type :ol.llx.agent.signal/abort})]
    (testing "transitions to idle and clears pending-tool-calls"
      (is (= :ol.llx.agent.loop/idle (:ol.llx.agent.loop/phase state')))
      (is (= [] (:pending-tool-calls state'))))

    (testing "emits turn-end"
      (is (= [:emit-event] (fx-types effects)))
      (is (= :ol.llx.agent.event/turn-end (get-in effects [0 :event :type]))))))

(deftest tool-executing-transition-unknown-signal-test
  (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/tool-executing
                                      :pending-tool-calls      [{:id "tc-1"}]})
        [state' effects] (sut/tool-executing-transition state {:type :ol.llx.agent.signal/llm-start})]
    (is (= state state'))
    (is (= [] effects))))

(deftest closed-transition-is-terminal-test
  (let [state (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/closed})]
    (testing "any signal returns state unchanged with no effects"
      (is (= [state []] (sut/closed-transition state {:type :ol.llx.agent.signal/llm-done})))
      (is (= [state []] (sut/closed-transition state {:type :ol.llx.agent.signal/abort})))
      (is (= [state []] (sut/closed-transition state {:type :ol.llx.agent.signal/tool-result}))))))

(deftest route-from-idle-test
  (is (= :ol.llx.agent.loop/idle (sut/route-from-idle {:ol.llx.agent.loop/phase :ol.llx.agent.loop/idle})))
  (is (= :ol.llx.agent.loop/streaming (sut/route-from-idle {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming})))
  (is (= :ol.llx.agent.loop/closed (sut/route-from-idle {:ol.llx.agent.loop/phase :ol.llx.agent.loop/closed}))))

(deftest route-from-streaming-test
  (testing "idle routes to idle"
    (is (= :ol.llx.agent.loop/idle
           (sut/route-from-streaming {:ol.llx.agent.loop/phase :ol.llx.agent.loop/idle}))))

  (testing "tool-executing routes to tool-executing"
    (is (= :ol.llx.agent.loop/tool-executing
           (sut/route-from-streaming {:ol.llx.agent.loop/phase :ol.llx.agent.loop/tool-executing}))))

  (testing "closed routes to closed"
    (is (= :ol.llx.agent.loop/closed
           (sut/route-from-streaming {:ol.llx.agent.loop/phase :ol.llx.agent.loop/closed}))))

  (testing "streaming stays streaming"
    (is (= :ol.llx.agent.loop/streaming
           (sut/route-from-streaming {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming})))))

(deftest route-from-tool-executing-test
  (testing "with pending tool calls stays tool-executing"
    (is (= :ol.llx.agent.loop/tool-executing
           (sut/route-from-tool-executing {:pending-tool-calls [{:id "tc-1"}]}))))

  (testing "without pending tool calls routes to streaming"
    (is (= :ol.llx.agent.loop/streaming
           (sut/route-from-tool-executing {:pending-tool-calls []}))))

  (testing "closed routes to closed"
    (is (= :ol.llx.agent.loop/closed
           (sut/route-from-tool-executing {:ol.llx.agent.loop/phase :ol.llx.agent.loop/closed
                                           :pending-tool-calls      []})))))

(deftest step-prompt-from-idle-to-streaming-test
  (let [state            (initial-state)
        [state' effects] (sut/step state {:type     :ol.llx.agent.command/prompt
                                          :messages [{:role :user :content "hello"}]})]
    (is (= :ol.llx.agent.loop/streaming (:ol.llx.agent.loop/phase state')))
    (is (= [{:role :user :content "hello"}] (:messages state')))
    (is (= :call-llm (::fx/type (last effects))))))

(deftest step-prompt-when-streaming-is-rejected-test
  (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming})
        [state' effects] (sut/step state {:type     :ol.llx.agent.command/prompt
                                          :messages [{:role :user :content "hi"}]})]
    (is (= :ol.llx.agent.loop/streaming (:ol.llx.agent.loop/phase state')))
    (is (= [] effects))))

(deftest step-signal-llm-done-no-tools-to-idle-test
  (let [message          {:role :assistant :content [{:type :text :text "Hi!"}]}
        state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                      :messages                [{:role :user :content "hello"}]
                                      :stream-message          {:role :assistant :content "partial"}})
        [state' effects] (sut/step state {:type :ol.llx.agent.signal/llm-done :message message})]
    (is (= :ol.llx.agent.loop/idle (:ol.llx.agent.loop/phase state')))
    (is (some #(= :ol.llx.agent.event/agent-end (get-in % [:event :type])) effects))))

(deftest step-signal-llm-done-with-tools-test
  (let [tool-call        (tool-call-block "tc-1" "read_file" {:path "/tmp"})
        message          {:role :assistant :content [{:type :text :text "Let me check."} tool-call]}
        state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                      :messages                [{:role :user :content "do stuff"}]})
        [state' effects] (sut/step state {:type :ol.llx.agent.signal/llm-done :message message})]
    (is (= :ol.llx.agent.loop/tool-executing (:ol.llx.agent.loop/phase state')))
    (is (some #(= :execute-tool (::fx/type %)) effects))))

(deftest step-abort-from-streaming-to-idle-test
  (testing "abort signal from streaming"
    (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                        :messages                [{:role :user :content "hello"}]})
          [state' effects] (sut/step state {:type :ol.llx.agent.signal/abort})]
      (is (= :ol.llx.agent.loop/idle (:ol.llx.agent.loop/phase state')))
      (is (= 1 (count (filter #(= :ol.llx.agent.event/agent-end (get-in % [:event :type])) effects))))))

  (testing "abort command from streaming"
    (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                        :messages                [{:role :user :content "hello"}]})
          [state' effects] (sut/step state {:type :ol.llx.agent.command/abort})]
      (is (= :ol.llx.agent.loop/idle (:ol.llx.agent.loop/phase state')))
      (is (= 1 (count (filter #(= :ol.llx.agent.event/agent-end (get-in % [:event :type])) effects)))))))

(deftest step-abort-from-tool-executing-to-idle-test
  (let [state            (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/tool-executing
                                      :pending-tool-calls      [{:id "tc-1"}]
                                      :messages                [{:role :user :content "x"}]})
        [state' effects] (sut/step state {:type :ol.llx.agent.command/abort})]
    (is (= :ol.llx.agent.loop/idle (:ol.llx.agent.loop/phase state')))
    (is (= [] (:pending-tool-calls state')))
    (is (= 1 (count (filter #(= :ol.llx.agent.event/agent-end (get-in % [:event :type])) effects))))))

(deftest step-full-happy-path-test
  (let [state         (initial-state)
        user-msg      {:role :user :content "hello"}
        assistant-msg {:role :assistant :content [{:type :text :text "Hi!"}]}

        [s1 _]        (sut/step state {:type :ol.llx.agent.command/prompt :messages [user-msg]})
        [s2 fx2]      (sut/step s1 {:type    :ol.llx.agent.signal/llm-start
                                    :message {:role :assistant :content [{:type :text :text ""}]}})
        [s3 fx3]      (sut/step s2 {:type  :ol.llx.agent.signal/llm-chunk
                                    :chunk {:role :assistant :content [{:type :text :text "Hi"}]}})
        [s4 fx4]      (sut/step s3 {:type :ol.llx.agent.signal/llm-done :message assistant-msg})]

    (testing "state progression"
      (is (= :ol.llx.agent.loop/streaming (:ol.llx.agent.loop/phase s1)))
      (is (= :ol.llx.agent.loop/streaming (:ol.llx.agent.loop/phase s2)))
      (is (= :ol.llx.agent.loop/streaming (:ol.llx.agent.loop/phase s3)))
      (is (= :ol.llx.agent.loop/idle (:ol.llx.agent.loop/phase s4))))

    (testing "final state has both messages"
      (is (= [user-msg assistant-msg] (:messages s4))))

    (testing "agent-end is emitted on final step"
      (is (some #(= :ol.llx.agent.event/agent-end (get-in % [:event :type])) fx4)))

    (testing "agent-end is not emitted on intermediate steps"
      (is (not (some #(= :ol.llx.agent.event/agent-end (get-in % [:event :type])) fx2)))
      (is (not (some #(= :ol.llx.agent.event/agent-end (get-in % [:event :type])) fx3))))))

(deftest step-tool-loop-test
  (let [state               (initial-state)
        user-msg            {:role :user :content "read file"}
        tool-call           (tool-call-block "tc-1" "read_file" {:path "/tmp"})
        assistant-with-tool {:role :assistant :content [{:type :text :text "Reading..."} tool-call]}
        tool-result         (tool-result-message "tc-1" "read_file" "file data" false)
        tool-result-msg     tool-result
        final-assistant     {:role :assistant :content [{:type :text :text "Here is the file."}]}

        [s1 _]              (sut/step state {:type :ol.llx.agent.command/prompt :messages [user-msg]})
        [s2 _]              (sut/step s1 {:type :ol.llx.agent.signal/llm-done :message assistant-with-tool})
        [s3 fx3b]           (sut/step s2 {:type                :ol.llx.agent.signal/tool-result
                                          :result              tool-result
                                          :tool-result-message tool-result-msg})
        [s4 fx4]            (sut/step s3 {:type :ol.llx.agent.signal/llm-done :message final-assistant})]

    (testing "state progression through tool loop"
      (is (= :ol.llx.agent.loop/streaming (:ol.llx.agent.loop/phase s1)))
      (is (= :ol.llx.agent.loop/tool-executing (:ol.llx.agent.loop/phase s2)))
      (is (= :ol.llx.agent.loop/streaming (:ol.llx.agent.loop/phase s3)))
      (is (= :ol.llx.agent.loop/idle (:ol.llx.agent.loop/phase s4))))

    (testing "tool completion resumes llm"
      (is (some #(= :call-llm (::fx/type %)) fx3b)))

    (testing "final state has all messages"
      (is (= [user-msg assistant-with-tool tool-result final-assistant]
             (:messages s4))))

    (testing "agent-end emitted on final transition to idle"
      (is (some #(= :ol.llx.agent.event/agent-end (get-in % [:event :type])) fx4)))))

(deftest agent-end-emitted-on-streaming-to-idle-test
  (let [state       (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                 :messages                [{:role :user :content "hello"}]})
        [_ effects] (sut/step state {:type    :ol.llx.agent.signal/llm-done
                                     :message {:role :assistant :content [{:type :text :text "Hi!"}]}})]
    (is (some #(= :ol.llx.agent.event/agent-end (get-in % [:event :type])) effects))))

(deftest agent-end-emitted-on-error-to-idle-test
  (let [state       (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                 :messages                [{:role :user :content "hello"}]})
        [_ effects] (sut/step state {:type :ol.llx.agent.signal/llm-error :error "boom"})]
    (is (some #(= :ol.llx.agent.event/agent-end (get-in % [:event :type])) effects))))

(deftest agent-end-not-emitted-when-staying-idle-test
  (let [state       (initial-state)
        [_ effects] (sut/step state {:type :ol.llx.agent.command/set-model :model {:id "gpt-4o"}})]
    (is (not (some #(= :ol.llx.agent.event/agent-end (get-in % [:event :type])) effects)))))

(deftest agent-end-not-emitted-on-streaming-to-tool-executing-test
  (let [tool-call   (tool-call-block "tc-1" "echo" {})
        state       (state-with {:ol.llx.agent.loop/phase :ol.llx.agent.loop/streaming
                                 :messages                [{:role :user :content "x"}]})
        [_ effects] (sut/step state {:type    :ol.llx.agent.signal/llm-done
                                     :message {:role :assistant :content [tool-call]}})]
    (is (not (some #(= :ol.llx.agent.event/agent-end (get-in % [:event :type])) effects)))))
