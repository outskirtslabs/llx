(ns ol.llx.agent.driver-test
  (:require
   [clojure.test :refer [deftest is]]
   [ol.llx.agent :as agent]
   [ol.llx.ai :as ai]
   [ol.llx.agent.loop :as loop]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(set! *warn-on-reflection* true)

(defn- zero-usage
  []
  {:input        0
   :output       0
   :cache-read   0
   :cache-write  0
   :total-tokens 0
   :cost         {:input 0 :output 0 :cache-read 0 :cache-write 0 :total 0}})

(defn- assistant-message
  [model text timestamp]
  {:role        :assistant
   :content     [{:type :text :text text}]
   :api         (:api model)
   :provider    (:provider model)
   :model       (:id model)
   :usage       (zero-usage)
   :stop-reason :stop
   :timestamp   timestamp})

(defn- stream-with-events
  [events]
  (let [ch (sp/chan :buf 8)]
    (future
      (doseq [event events]
        (sp/offer ch event))
      (sp/close ch))
    ch))

(defn- collect-until-agent-end!
  [events>]
  (loop [events []]
    (let [event (sp/take! events> 1000 ::timeout)]
      (cond
        (= ::timeout event)
        (throw (ex-info "Timed out waiting for agent-end event"
                        {:event-count (count events)}))

        (nil? event)
        events

        (= :ol.llx.agent.event/agent-end (:type event))
        (conj events event)

        :else
        (recur (conj events event))))))

(defn- wait-for-value!
  [value-fn timeout-ms label]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if-let [value (value-fn)]
        value
        (if (< (System/currentTimeMillis) deadline)
          (do
            (Thread/sleep 10)
            (recur))
          (throw (ex-info (str "Timed out waiting for " label)
                          {:label label :timeout-ms timeout-ms})))))))

(deftest prompt-continues-through-queued-steering-without-stalling-test
  (let [model                 (ai/get-model :openai "gpt-4o")
        first-stream-release* (promise)
        first-stream-started* (promise)
        call-count*           (atom 0)
        stream-fn             (fn [stream-model _context _opts]
                                (let [call-number (swap! call-count* inc)]
                                  (cond
                                    (= 1 call-number)
                                    (let [ch (sp/chan :buf 8)]
                                      (future
                                        (sp/offer ch {:type :start})
                                        (deliver first-stream-started* true)
                                        (deref first-stream-release* 1000 ::timeout)
                                        (sp/offer ch {:type              :done
                                                      :assistant-message (assistant-message stream-model "first" 2)})
                                        (sp/close ch))
                                      ch)

                                    (= 2 call-number)
                                    (stream-with-events
                                     [{:type :start}
                                      {:type              :done
                                       :assistant-message (assistant-message stream-model "second" 4)}])

                                    :else
                                    (throw (ex-info "Unexpected stream-fn invocation"
                                                    {:call-number call-number})))))
        runtime               (agent/create-agent {:model             model
                                                   :tools             []
                                                   :convert-to-llm    identity
                                                   :transform-context (fn [messages _abort-signal]
                                                                        messages)
                                                   :stream-fn         stream-fn})
        events>               (agent/subscribe runtime)]
    (try
      (let [prompt* (agent/prompt runtime [{:role :user :content "hello" :timestamp 1}])]
        (is (true? (deref first-stream-started* 1000 false)))
        (p/await (agent/steer runtime [{:role :user :content "continue with this" :timestamp 3}]))
        (deliver first-stream-release* true)
        (p/await prompt*)
        (is (true? (p/await (agent/wait-for-idle runtime 1000))))
        (let [events             (collect-until-agent-end! events>)
              final-state        (agent/state runtime)
              messages           (:messages final-state)
              assistant-messages (filter #(= :assistant (:role %)) messages)
              user-messages      (filter #(= :user (:role %)) messages)]
          (is (= :ol.llx.agent.loop/idle (:ol.llx.agent.loop/phase final-state)))
          (is (= 2 @call-count*))
          (is (= [:user :assistant :user :assistant]
                 (mapv :role messages)))
          (is (= ["hello" "continue with this"]
                 (mapv :content user-messages)))
          (is (= ["first" "second"]
                 (mapv #(get-in % [:content 0 :text]) assistant-messages)))
          (is (= 2 (count (filter #(= :ol.llx.agent.event/turn-start (:type %)) events))))
          (is (= 1 (count (filter #(= :ol.llx.agent.event/agent-end (:type %)) events))))))
      (finally
        (agent/unsubscribe runtime events>)
        (p/await (agent/close runtime))))))

(deftest abort-drops-stale-llm-events-and-allows-reuse-test
  (let [model       (ai/get-model :openai "gpt-4o")
        stream-ch*  (atom nil)
        call-count* (atom 0)
        stream-fn   (fn [_stream-model _context _opts]
                      (swap! call-count* inc)
                      (let [ch (sp/chan :buf 8)]
                        (reset! stream-ch* ch)
                        ch))
        runtime     (agent/create-agent {:model          model
                                         :tools          []
                                         :convert-to-llm identity
                                         :stream-fn      stream-fn})
        events>     (agent/subscribe runtime)]
    (try
      (let [prompt*      (agent/prompt runtime [{:role :user :content "hello" :timestamp 1}])
            first-stream (wait-for-value! #(deref stream-ch*) 1000 "first stream channel")]
        (sp/offer first-stream {:type :start})
        (is (true? (p/await (agent/abort runtime))))
        (sp/offer first-stream {:type              :done
                                :assistant-message (assistant-message model "late" 2)})
        (sp/close first-stream)
        (is (true? (p/await prompt*)))
        (is (true? (p/await (agent/wait-for-idle runtime 1000))))
        (let [state (agent/state runtime)]
          (is (= ::loop/idle (::loop/phase state)))
          (is (= [{:role :user :content "hello" :timestamp 1}]
                 (:messages state))))
        (let [prompt*       (agent/prompt runtime [{:role :user :content "again" :timestamp 3}])
              second-stream (wait-for-value! #(let [ch @stream-ch*]
                                                (when (and ch (not= ch first-stream))
                                                  ch))
                                             1000
                                             "second stream channel")]
          (sp/offer second-stream {:type :start})
          (sp/offer second-stream {:type              :done
                                   :assistant-message (assistant-message model "fresh" 4)})
          (sp/close second-stream)
          (is (true? (p/await prompt*))))
        (is (true? (p/await (agent/wait-for-idle runtime 1000))))
        (let [state              (agent/state runtime)
              assistant-messages (filter #(= :assistant (:role %)) (:messages state))]
          (is (= 2 @call-count*))
          (is (= ["fresh"] (mapv #(get-in % [:content 0 :text]) assistant-messages)))))
      (finally
        (agent/unsubscribe runtime events>)
        (p/await (agent/close runtime))))))

(deftest abort-cancels-tool-execution-and-returns-to-idle-test
  (let [model         (ai/get-model :openai "gpt-4o")
        stream-ch*    (atom nil)
        tool-aborted* (promise)
        tool-ran*     (atom 0)
        blocking-tool {:name         "slow_tool"
                       :description  "Blocks until cancelled."
                       :input-schema [:map]
                       :execute      (fn [_id _args abort-signal _on-update]
                                       (swap! tool-ran* inc)
                                       (let [result (p/deferred)]
                                         (future
                                           (loop []
                                             (if @abort-signal
                                               (do
                                                 (deliver tool-aborted* true)
                                                 (p/resolve result {:content [{:type :text :text "tool finished"}]}))
                                               (do
                                                 (Thread/sleep 10)
                                                 (recur)))))
                                         result))}
        runtime       (agent/create-agent {:model          model
                                           :tools          [blocking-tool]
                                           :convert-to-llm identity
                                           :stream-fn      (fn [_stream-model _context _opts]
                                                             (let [ch (sp/chan :buf 8)]
                                                               (reset! stream-ch* ch)
                                                               ch))})
        prompt*       (agent/prompt runtime [{:role :user :content "run tool" :timestamp 1}])]
    (try
      (let [stream-ch (wait-for-value! #(deref stream-ch*) 1000 "tool stream channel")]
        (sp/offer stream-ch {:type :start})
        (sp/offer stream-ch {:type              :done
                             :assistant-message {:role        :assistant
                                                 :content     [{:type      :tool-call
                                                                :id        "tc-1"
                                                                :name      "slow_tool"
                                                                :arguments {}}]
                                                 :api         (:api model)
                                                 :provider    (:provider model)
                                                 :model       (:id model)
                                                 :usage       (zero-usage)
                                                 :stop-reason :tool-use
                                                 :timestamp   1}})
        (sp/close stream-ch))
      (is (true? (wait-for-value! #(when (= ::loop/tool-executing (::loop/phase (agent/state runtime)))
                                     true)
                                  3000
                                  "tool execution")))
      (is (true? (p/await (agent/abort runtime))))
      (is (true? (deref tool-aborted* 1000 false)))
      (is (true? (p/await prompt*)))
      (is (true? (p/await (agent/wait-for-idle runtime 1000))))
      (is (= 1 @tool-ran*))
      (is (= ::loop/idle (::loop/phase (agent/state runtime))))
      (finally
        (p/await (agent/close runtime))))))
