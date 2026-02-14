(ns llx.agent.loop
  "Low-level agent loop API implemented in shared CLJC.

  This module provides the production turn loop with event emission, tool
  execution, steering/follow-up queue hooks, and continue semantics."
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [llx.ai :as ai]
   [llx.agent.schema :as agent-schema]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(defn- now-ms
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

(defn- empty-usage
  []
  {:input        0
   :output       0
   :cache-read   0
   :cache-write  0
   :total-tokens 0
   :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}})

(defn- default-convert-to-llm
  [messages]
  (filterv #(contains? #{:user :assistant :tool-result} (:role %)) messages))

(defn- stream-options
  [config]
  (select-keys config
               [:max-tokens
                :temperature
                :top-p
                :reasoning
                :reasoning-effort
                :signal
                :headers
                :metadata
                :registry]))

(defn- ai-tools
  [tools]
  (mapv #(select-keys % [:name :description :input-schema]) tools))

(defn- emit!
  [out event]
  (let [event (agent-schema/validate! :llx.agent/event event)]
    (-> (sp/put out event)
        (p/then boolean))))

(defn- partial-assistant
  [model]
  {:role        :assistant
   :content     []
   :api         (:api model)
   :provider    (:provider model)
   :model       (:id model)
   :usage       (empty-usage)
   :stop-reason :stop
   :timestamp   (now-ms)})

(defn- append-block
  [assistant block]
  (update assistant :content conj block))

(defn- update-last-block
  [assistant expected-type update-fn]
  (let [idx (dec (count (:content assistant)))
        blk (when (<= 0 idx) (nth (:content assistant) idx nil))]
    (if (= expected-type (:type blk))
      (assoc-in assistant [:content idx] (update-fn blk))
      assistant)))

(defn- apply-stream-event
  [assistant stream-event]
  (case (:type stream-event)
    :text-start
    (append-block assistant {:type :text :text ""})

    :text-delta
    (update-last-block assistant :text
                       (fn [blk]
                         (update blk :text str (:text stream-event))))

    :thinking-start
    (append-block assistant {:type :thinking :thinking ""})

    :thinking-delta
    (update-last-block assistant :thinking
                       (fn [blk]
                         (update blk :thinking str (:thinking stream-event))))

    :toolcall-start
    (append-block assistant {:type      :tool-call
                             :id        (:id stream-event)
                             :name      (:name stream-event)
                             :arguments {}})

    :toolcall-delta
    (update-last-block assistant :tool-call
                       (fn [blk]
                         (assoc blk :arguments (:arguments stream-event))))

    :toolcall-end
    (update-last-block assistant :tool-call
                       (fn [blk]
                         (assoc blk :arguments (:arguments stream-event))))

    assistant))

(defn- invoke-stream
  [env model context config]
  (let [stream-fn      (or (:stream-fn config) ai/stream)
        opts           (stream-options config)
        api-key-source (:get-api-key config)]
    (-> (p/resolved nil)
        (p/then (fn [_]
                  (if (fn? api-key-source)
                    (api-key-source (:provider model))
                    nil)))
        (p/then (fn [api-key]
                  (let [opts (cond-> opts
                               (string? api-key) (assoc :api-key api-key))]
                    (stream-fn env model context opts)))))))

(defn- stream-assistant-response!
  [out context config]
  (let [env               (or (:env config) (ai/default-env))
        model             (:model config)
        convert-to-llm    (or (:convert-to-llm config) default-convert-to-llm)
        transform-context (:transform-context config)]
    (-> (p/resolved (:messages context))
        (p/then (fn [messages]
                  (if (fn? transform-context)
                    (transform-context messages (:signal config))
                    messages)))
        (p/then (fn [messages]
                  (convert-to-llm messages)))
        (p/then (fn [llm-messages]
                  (let [llm-context {:system-prompt (:system-prompt context)
                                     :messages      (vec llm-messages)
                                     :tools         (ai-tools (:tools context))}]
                    (invoke-stream env model llm-context config))))
        (p/then
         (fn [stream-ch]
           ^{:clj-kondo/ignore [:loop-without-recur]}
           (p/loop [state {:partial nil :started? false}]
             (let [{:keys [partial started?]} state]
               (p/let [stream-event (sp/take stream-ch)]
                 (cond
                   (nil? stream-event)
                   (if started?
                     (p/resolved partial)
                     (p/rejected (ex-info "Stream closed before terminal event."
                                          {:type :llx.agent.loop/stream-closed})))

                   (= :start (:type stream-event))
                   (let [partial (partial-assistant model)]
                     (-> (emit! out {:llx.agent.event/type    :message-start
                                     :llx.agent.event/message partial})
                         (p/then (fn [_]
                                   (p/recur {:partial partial :started? true})))))

                   (#{:done :error} (:type stream-event))
                   (let [assistant (agent-schema/validate! :llx/message-assistant
                                                           (:assistant-message stream-event))]
                     (-> (if started?
                           (p/resolved true)
                           (emit! out {:llx.agent.event/type    :message-start
                                       :llx.agent.event/message assistant}))
                         (p/then (fn [_]
                                   (emit! out {:llx.agent.event/type    :message-end
                                               :llx.agent.event/message assistant})))
                         (p/then (fn [_]
                                   assistant))))

                   :else
                   (let [next-partial (apply-stream-event (or partial (partial-assistant model))
                                                          stream-event)]
                     (-> (emit! out {:llx.agent.event/type                    :message-update
                                     :llx.agent.event/message                 next-partial
                                     :llx.agent.event/assistant-message-event stream-event})
                         (p/then (fn [_]
                                   (p/recur {:partial next-partial :started? true}))))))))))))))

(defn- normalize-tool-result
  [result]
  (agent-schema/validate! :llx.agent/tool-result result))

(defn- tool-error-result
  [error]
  {:content [{:type :text :text (or (ex-message error)
                                    "Tool execution failed.")}]
   :details {}})

(defn- invoke-tool-execute
  [execute tool-call-id params signal on-update]
  (try
    (execute tool-call-id params signal on-update)
    (catch #?(:clj clojure.lang.ArityException :cljs :default) _
      (try
        (execute tool-call-id params signal)
        (catch #?(:clj clojure.lang.ArityException :cljs :default) _
          (try
            (execute tool-call-id params)
            (catch #?(:clj clojure.lang.ArityException :cljs :default) _
              (execute params))))))))

(defn- tool-result-message
  [tool-call result is-error?]
  {:role         :tool-result
   :tool-call-id (:id tool-call)
   :tool-name    (:name tool-call)
   :content      (:content result)
   :is-error?    (boolean is-error?)
   :timestamp    (now-ms)})

(defn- emit-tool-result-message!
  [out message]
  (-> (emit! out {:llx.agent.event/type    :message-start
                  :llx.agent.event/message message})
      (p/then (fn [_]
                (emit! out {:llx.agent.event/type    :message-end
                            :llx.agent.event/message message})))))

(defn- execute-tool-call!
  [out tools tool-call config]
  (let [signal (:signal config)]
    (-> (emit! out {:llx.agent.event/type         :tool-execution-start
                    :llx.agent.event/tool-call-id (:id tool-call)
                    :llx.agent.event/tool-name    (:name tool-call)
                    :llx.agent.event/args         (:arguments tool-call)})
        (p/then (fn [_]
                  (-> (p/resolved nil)
                      (p/then (fn [_]
                                (let [tool (first (filter #(= (:name %) (:name tool-call)) tools))]
                                  (when-not tool
                                    (throw (ex-info (str "Tool " (:name tool-call) " not found")
                                                    {:type :llx.agent.loop/tool-not-found
                                                     :tool (:name tool-call)})))
                                  (let [params (ai/validate-tool-call tools tool-call)]
                                    (invoke-tool-execute
                                     (:execute tool)
                                     (:id tool-call)
                                     params
                                     signal
                                     (fn [partial-result]
                                       (emit! out {:llx.agent.event/type           :tool-execution-update
                                                   :llx.agent.event/tool-call-id   (:id tool-call)
                                                   :llx.agent.event/tool-name      (:name tool-call)
                                                   :llx.agent.event/args           (:arguments tool-call)
                                                   :llx.agent.event/partial-result partial-result})))))))
                      (p/then (fn [result]
                                {:result   (normalize-tool-result result)
                                 :is-error false}))
                      (p/catch (fn [error]
                                 {:result   (tool-error-result error)
                                  :is-error true})))))
        (p/then (fn [{:keys [result is-error]}]
                  (-> (emit! out {:llx.agent.event/type         :tool-execution-end
                                  :llx.agent.event/tool-call-id (:id tool-call)
                                  :llx.agent.event/tool-name    (:name tool-call)
                                  :llx.agent.event/result       result
                                  :llx.agent.event/error?       is-error})
                      (p/then (fn [_]
                                (let [message (tool-result-message tool-call result is-error)]
                                  (-> (emit-tool-result-message! out message)
                                      (p/then (fn [_]
                                                {:tool-result-message message
                                                 :tool-result         result
                                                 :is-error            is-error}))))))))))))

(defn- skip-tool-call!
  [out tool-call]
  (let [result  {:content [{:type :text :text "Skipped due to queued user message."}]
                 :details {}}
        message (tool-result-message tool-call result true)]
    (-> (emit! out {:llx.agent.event/type         :tool-execution-start
                    :llx.agent.event/tool-call-id (:id tool-call)
                    :llx.agent.event/tool-name    (:name tool-call)
                    :llx.agent.event/args         (:arguments tool-call)})
        (p/then (fn [_]
                  (emit! out {:llx.agent.event/type         :tool-execution-end
                              :llx.agent.event/tool-call-id (:id tool-call)
                              :llx.agent.event/tool-name    (:name tool-call)
                              :llx.agent.event/result       result
                              :llx.agent.event/error?       true})))
        (p/then (fn [_]
                  (emit-tool-result-message! out message)))
        (p/then (fn [_]
                  message)))))

(defn- inject-messages!
  [out messages context new-messages]
  (reduce
   (fn [acc message]
     (-> acc
         (p/then (fn [{:keys [context new-messages]}]
                   (-> (emit! out {:llx.agent.event/type    :message-start
                                   :llx.agent.event/message message})
                       (p/then (fn [_]
                                 (emit! out {:llx.agent.event/type    :message-end
                                             :llx.agent.event/message message})))
                       (p/then (fn [_]
                                 {:context      (update context :messages conj message)
                                  :new-messages (conj new-messages message)})))))))
   (p/resolved {:context context :new-messages new-messages})
   messages))

(defn- execute-tool-calls!
  [out tool-calls context new-messages config]
  (let [tools                 (:tools context)
        get-steering-messages (:get-steering-messages config)]
    (letfn [(continue-step [idx tool-results context new-messages]
              {:continue?    true
               :next-idx     (inc idx)
               :tool-results tool-results
               :context      context
               :new-messages new-messages})

            (skip-remaining [remaining tool-results context new-messages]
              (reduce
               (fn [acc remaining-tool-call]
                 (-> acc
                     (p/then
                      (fn [{:keys [tool-results context new-messages]}]
                        (-> (skip-tool-call! out remaining-tool-call)
                            (p/then
                             (fn [skipped-message]
                               {:tool-results (conj tool-results skipped-message)
                                :context      (update context :messages conj skipped-message)
                                :new-messages (conj new-messages skipped-message)})))))))
               (p/resolved {:tool-results tool-results
                            :context      context
                            :new-messages new-messages})
               remaining))

            (step [{:keys [idx tool-results context new-messages]}]
              (if (>= idx (count tool-calls))
                (p/resolved {:tool-results      tool-results
                             :steering-messages []
                             :context           context
                             :new-messages      new-messages})
                (let [tool-call (nth tool-calls idx)]
                  (-> (execute-tool-call! out tools tool-call config)
                      (p/then
                       (fn [{:keys [tool-result-message]}]
                         (let [tool-results (conj tool-results tool-result-message)
                               context      (update context :messages conj tool-result-message)
                               new-messages (conj new-messages tool-result-message)]
                           (if (fn? get-steering-messages)
                             (-> (p/resolved (or (get-steering-messages) []))
                                 (p/then
                                  (fn [steering-messages]
                                    (if (seq steering-messages)
                                      (-> (skip-remaining (subvec (vec tool-calls) (inc idx))
                                                          tool-results
                                                          context
                                                          new-messages)
                                          (p/then
                                           (fn [{:keys [tool-results context new-messages]}]
                                             {:done?             true
                                              :tool-results      tool-results
                                              :steering-messages (vec steering-messages)
                                              :context           context
                                              :new-messages      new-messages})))
                                      (continue-step idx tool-results context new-messages)))))
                             (p/resolved (continue-step idx tool-results context new-messages))))))
                      (p/then
                       (fn [step-result]
                         (if (:continue? step-result)
                           (step {:idx          (:next-idx step-result)
                                  :tool-results (:tool-results step-result)
                                  :context      (:context step-result)
                                  :new-messages (:new-messages step-result)})
                           {:tool-results      (:tool-results step-result)
                            :steering-messages (:steering-messages step-result)
                            :context           (:context step-result)
                            :new-messages      (:new-messages step-result)})))))))]
      (step {:idx          0
             :tool-results []
             :context      context
             :new-messages new-messages}))))
(defn- run-turn-step!
  [out first-turn? pending-messages context new-messages config]
  (let [get-steering-messages (:get-steering-messages config)]
    (-> (if first-turn?
          (p/resolved true)
          (emit! out {:llx.agent.event/type :turn-start}))
        (p/then (fn [_]
                  (inject-messages! out pending-messages context new-messages)))
        (p/then (fn [{:keys [context new-messages]}]
                  (-> (stream-assistant-response! out context config)
                      (p/then (fn [assistant-message]
                                (let [context      (update context :messages conj assistant-message)
                                      new-messages (conj new-messages assistant-message)]
                                  (if (contains? #{:error :aborted} (:stop-reason assistant-message))
                                    (-> (emit! out {:llx.agent.event/type         :turn-end
                                                    :llx.agent.event/message      assistant-message
                                                    :llx.agent.event/tool-results []})
                                        (p/then (fn [_]
                                                  (emit! out {:llx.agent.event/type     :agent-end
                                                              :llx.agent.event/messages new-messages})))
                                        (p/then (fn [_]
                                                  {:terminal?           true
                                                   :context             context
                                                   :new-messages        new-messages
                                                   :pending-messages    []
                                                   :has-more-tool-calls false})))
                                    (let [tool-calls (vec (filter #(= :tool-call (:type %))
                                                                  (:content assistant-message)))]
                                      (-> (if (seq tool-calls)
                                            (execute-tool-calls! out
                                                                 tool-calls
                                                                 context
                                                                 new-messages
                                                                 config)
                                            (p/resolved {:tool-results      []
                                                         :steering-messages []
                                                         :context           context
                                                         :new-messages      new-messages}))
                                          (p/then (fn [{:keys [tool-results steering-messages context new-messages]}]
                                                    (-> (emit! out {:llx.agent.event/type         :turn-end
                                                                    :llx.agent.event/message      assistant-message
                                                                    :llx.agent.event/tool-results tool-results})
                                                        (p/then (fn [_]
                                                                  (if (seq steering-messages)
                                                                    steering-messages
                                                                    (if (fn? get-steering-messages)
                                                                      (or (get-steering-messages) [])
                                                                      []))))
                                                        (p/then (fn [next-pending]
                                                                  {:terminal?           false
                                                                   :context             context
                                                                   :new-messages        new-messages
                                                                   :pending-messages    next-pending
                                                                   :has-more-tool-calls (seq tool-calls)})))))))))))))))))

(defn- run-loop!
  [out context new-messages config]
  (let [get-steering-messages  (:get-steering-messages config)
        get-follow-up-messages (:get-follow-up-messages config)
        initial-pending        (if (fn? get-steering-messages)
                                 (or (get-steering-messages) [])
                                 [])]
    (letfn [(step [{:keys [first-turn? pending-messages has-more-tool-calls context new-messages]}]
              (if (or has-more-tool-calls (seq pending-messages))
                (-> (run-turn-step! out first-turn? pending-messages context new-messages config)
                    (p/then (fn [{:keys [terminal?
                                         context
                                         new-messages
                                         pending-messages
                                         has-more-tool-calls]}]
                              (if terminal?
                                {:terminal?    true
                                 :context      context
                                 :new-messages new-messages}
                                (step {:first-turn?         false
                                       :pending-messages    pending-messages
                                       :has-more-tool-calls has-more-tool-calls
                                       :context             context
                                       :new-messages        new-messages})))))
                (let [follow-up-messages (if (fn? get-follow-up-messages)
                                           (or (get-follow-up-messages) [])
                                           [])]
                  (if (seq follow-up-messages)
                    (step {:first-turn?         false
                           :pending-messages    follow-up-messages
                           :has-more-tool-calls false
                           :context             context
                           :new-messages        new-messages})
                    (-> (emit! out {:llx.agent.event/type     :agent-end
                                    :llx.agent.event/messages new-messages})
                        (p/then (fn [_]
                                  {:terminal?    false
                                   :context      context
                                   :new-messages new-messages})))))))]
      (step {:first-turn?         true
             :pending-messages    initial-pending
             :has-more-tool-calls true
             :context             context
             :new-messages        new-messages}))))

(>defn agent-loop
       "Runs a new agent loop turn from `prompts`, emitting `llx.agent` events.

  Returns a channel of agent events. The channel closes after terminal
  `:agent-end`."
       [prompts context config]
       [[:vector :llx.agent/message] :llx.agent/context :llx.agent/loop-config => :llx.agent/channel]
       (let [prompts (vec prompts)
             context (agent-schema/validate! :llx.agent/context context)
             config  (agent-schema/validate! :llx.agent/loop-config config)
             out     (sp/chan 64)]
         (-> (emit! out {:llx.agent.event/type :agent-start})
             (p/then (fn [_]
                       (emit! out {:llx.agent.event/type :turn-start})))
             (p/then (fn [_]
                       (inject-messages! out prompts context [])))
             (p/then (fn [{:keys [context]}]
                       (run-loop! out context prompts config)))
             (p/catch (fn [error]
                        (let [assistant-message
                              {:role          :assistant
                               :content       []
                               :api           (get-in config [:model :api])
                               :provider      (get-in config [:model :provider])
                               :model         (get-in config [:model :id])
                               :usage         (empty-usage)
                               :stop-reason   :error
                               :error-message (or (ex-message error)
                                                  "Agent loop failed.")
                               :timestamp     (now-ms)}]
                          (emit! out {:llx.agent.event/type     :agent-end
                                      :llx.agent.event/messages [assistant-message]}))))
             (p/finally (fn [_ _]
                          (sp/close out))))
         out))

(>defn agent-loop-continue
       "Continues an existing context without injecting new prompt messages."
       [context config]
       [:llx.agent/context :llx.agent/loop-config => :llx.agent/channel]
       (let [context (agent-schema/validate! :llx.agent/context context)]
         (when (empty? (:messages context))
           (throw (ex-info "Cannot continue: no messages in context"
                           {:type :llx.agent.loop/empty-context})))
         (when (= :assistant (:role (last (:messages context))))
           (throw (ex-info "Cannot continue from message role: assistant"
                           {:type :llx.agent.loop/assistant-tail})))
         (let [config (agent-schema/validate! :llx.agent/loop-config config)
               out    (sp/chan 64)]
           (-> (emit! out {:llx.agent.event/type :agent-start})
               (p/then (fn [_]
                         (emit! out {:llx.agent.event/type :turn-start})))
               (p/then (fn [_]
                         (run-loop! out context [] config)))
               (p/catch (fn [_]
                          (emit! out {:llx.agent.event/type     :agent-end
                                      :llx.agent.event/messages []})))
               (p/finally (fn [_ _]
                            (sp/close out))))
           out)))
