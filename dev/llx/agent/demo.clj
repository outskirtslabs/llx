(ns llx.agent.demo
  (:require
   [llx.agent.runtime.api :as agent]
   [llx.ai :as ai]
   [llx.ai.demo :as ai-demo]
   [llx.ai.impl.client.event-stream :as stream]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(set! *warn-on-reflection* true)

;; Reuse the AI demo OpenAI model.
(def demo-model ai-demo/openai-model)

(def demo-env
  (ai/default-env))

(def demo-agent-tools
  [{:name         "search"
    :label        "Search"
    :description  "Search demo tool"
    :input-schema [:map
                   [:query :string]]
    :execute      (fn [_args]
                    {:content [{:type :text :text "not implemented in this demo"}]
                     :details nil})}])

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn user-message
  [text]
  {:role      :user
   :content   [{:type :text :text text}]
   :timestamp (now-ms)})

(defn- normalize-messages
  [message-or-messages]
  (cond
    (nil? message-or-messages) []
    (vector? message-or-messages) message-or-messages
    :else [message-or-messages]))

(defn- ai-tools
  [agent-tools]
  (mapv #(select-keys % [:name :description :input-schema]) agent-tools))

(defn- build-context
  [{:keys [state messages]}]
  {:system-prompt (:system-prompt state)
   :messages      (vec (concat (:messages state)
                               (normalize-messages messages)))
   :tools         (ai-tools (:tools state))})

(defn- stream-error
  [assistant-message]
  (ex-info (or (:error-message assistant-message)
               "Streaming request failed.")
           {:type              :llx.agent/demo-stream-error
            :assistant-message assistant-message}))

(defn- consume-ai-stream!
  [stream-ch]
  (p/loop []
    (p/let [event (sp/take stream-ch)]
      (cond
        (nil? event)
        (p/rejected (ex-info "Stream closed before terminal event."
                             {:type :llx.agent/demo-stream-closed}))

        (= :done (:type event))
        (:assistant-message event)

        (= :error (:type event))
        (p/rejected (stream-error (:assistant-message event)))

        :else
        (p/recur)))))

(defn- emit-assistant-turn!
  [emit-event! assistant]
  (-> (emit-event! {:llx.agent.event/type    :message-start
                    :llx.agent.event/message assistant})
      (p/then (fn [_]
                (emit-event! {:llx.agent.event/type    :message-end
                              :llx.agent.event/message assistant})))
      (p/then (fn [_]
                (emit-event! {:llx.agent.event/type     :agent-end
                              :llx.agent.event/messages [assistant]})))))

(defn- emit-input-turn!
  [emit-event! messages]
  (reduce
   (fn [acc message]
     (-> acc
         (p/then (fn [_]
                   (emit-event! {:llx.agent.event/type    :message-start
                                 :llx.agent.event/message message})))
         (p/then (fn [_]
                   (emit-event! {:llx.agent.event/type    :message-end
                                 :llx.agent.event/message message})))))
   (p/resolved true)
   (normalize-messages messages)))

(defn demo-run-command!
  [{:keys [command emit-event! messages] :as input}]
  (let [stream-ch (ai/stream demo-env
                             demo-model
                             (build-context input)
                             {:max-tokens  256
                              :temperature 0.0})
        closed?*  (atom false)]
    {:result  (-> (emit-input-turn! emit-event! messages)
                  (p/then (fn [_]
                            (consume-ai-stream! stream-ch)))
                  (p/then (fn [assistant]
                            (-> (emit-assistant-turn! emit-event! assistant)
                                (p/then (fn [_]
                                          {:status    :ok
                                           :command   command
                                           :assistant assistant})))))
                  (p/catch (fn [ex]
                             (when (and (map? (ex-data ex))
                                        (= :llx.agent/demo-stream-error (:type (ex-data ex))))
                               (when-let [assistant (:assistant-message (ex-data ex))]
                                 (emit-event! {:llx.agent.event/type    :message-end
                                               :llx.agent.event/message assistant})))
                             (p/rejected ex))))
     :cancel! (fn []
                (when (compare-and-set! closed?* false true)
                  (sp/close stream-ch))
                nil)}))

(defn create-demo-runtime
  []
  (agent/create-runtime
   {:initial-state {:system-prompt  "You are the llx.agent demo runtime."
                    :model          demo-model
                    :tools          demo-agent-tools
                    :thinking-level :off}
    :run-command!  demo-run-command!}))

(defn await!
  [deferred]
  (stream/await! deferred))

(defn tap-state!
  [runtime]
  (tap> (agent/state runtime))
  runtime)

(comment
  ;; Requires OPENAI_API_KEY in env when using `demo-model` from `llx.ai.demo`.
  (def rt (create-demo-runtime))

  ;; `stop-events` is the unsubscribe function returned by `agent/subscribe`.
  (def stop-events
    (agent/subscribe
     rt
     (fn [event]
       (tap> event))))

  (await! (agent/prompt! rt (user-message "hello from llx.agent demo")))
  (tap-state! rt)

  (await! (agent/steer! rt (user-message "be concise")))
  (await! (agent/follow-up! rt (user-message "mention queue handling")))
  (tap-state! rt)

  (await! (agent/continue! rt))
  (tap-state! rt)

  (await! (agent/reset! rt))
  (tap-state! rt)

  ;; Call `stop-events` when you no longer want event callbacks for this runtime.
  (stop-events)
  (await! (agent/close! rt)))
