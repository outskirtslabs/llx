(ns llx.agent.fx.inference
  (:require
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [llx.ai :as ai]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(defn- base-partial-assistant-message
  [model]
  (cond-> {:role :assistant :content []}
    (:api model) (assoc :api (:api model))
    (:provider model) (assoc :provider (:provider model))
    (:id model) (assoc :model (:id model))))

(def LlmEventStepSignal
  [:multi {:dispatch :type}
   [:llx.agent.signal/rejected :llx.agent/signal-rejected]
   [:llx.agent.signal/llm-start :llx.agent/signal-llm-start]
   [:llx.agent.signal/llm-chunk :llx.agent/signal-llm-chunk]
   [:llx.agent.signal/llm-done :llx.agent/signal-llm-done]
   [:llx.agent.signal/llm-error :llx.agent/signal-llm-error]])

(def LlmEventStep
  [:map
   [:partial :llx.agent/partial-assistant-message]
   [:signal [:maybe LlmEventStepSignal]]
   [:done? :boolean]])

(defn- ensure-text-block
  [partial]
  (let [content (:content partial)]
    (if (and (seq content) (= :text (:type (peek content))))
      partial
      (update partial :content conj {:type :text :text ""}))))

(defn- ensure-thinking-block
  [partial]
  (let [content (:content partial)]
    (if (and (seq content) (= :thinking (:type (peek content))))
      partial
      (update partial :content conj {:type :thinking :thinking ""}))))

(defn- update-last-text
  [partial text-delta]
  (let [partial (ensure-text-block partial)]
    (update-in partial [:content (dec (count (:content partial))) :text]
               (fnil str "")
               (or text-delta ""))))

(defn- update-last-thinking
  [partial thinking-delta]
  (let [partial (ensure-thinking-block partial)]
    (update-in partial [:content (dec (count (:content partial))) :thinking]
               (fnil str "")
               (or thinking-delta ""))))

(defn- upsert-tool-call
  [partial {:keys [id name arguments]}]
  (let [content (:content partial)
        idx     (first (keep-indexed (fn [i block]
                                       (when (and (= :tool-call (:type block))
                                                  (= id (:id block)))
                                         i))
                                     content))
        block   {:type      :tool-call
                 :id        id
                 :name      name
                 :arguments (or arguments {})}]
    (if (some? idx)
      (assoc-in partial [:content idx] block)
      (update partial :content conj block))))

(>defn llm-event->step
       [model partial event]
       [:llx/model [:maybe :llx.agent/partial-assistant-message] :llx/event => LlmEventStep]
       (let [event-type (:type event)
             partial    (or partial
                            (base-partial-assistant-message model))]
         (case event-type
           :start
           {:partial partial
            :signal  {:type :llx.agent.signal/llm-start :message partial}
            :done?   false}

           :text-start
           (let [partial (ensure-text-block partial)]
             {:partial partial
              :signal  {:type :llx.agent.signal/llm-chunk :chunk partial}
              :done?   false})

           :text-delta
           (let [partial (update-last-text partial (:text event))]
             {:partial partial
              :signal  {:type :llx.agent.signal/llm-chunk :chunk partial}
              :done?   false})

           :text-end
           {:partial partial
            :signal  {:type :llx.agent.signal/llm-chunk :chunk partial}
            :done?   false}

           :thinking-start
           (let [partial (ensure-thinking-block partial)]
             {:partial partial
              :signal  {:type :llx.agent.signal/llm-chunk :chunk partial}
              :done?   false})

           :thinking-delta
           (let [partial (update-last-thinking partial (:thinking event))]
             {:partial partial
              :signal  {:type :llx.agent.signal/llm-chunk :chunk partial}
              :done?   false})

           :thinking-end
           {:partial partial
            :signal  {:type :llx.agent.signal/llm-chunk :chunk partial}
            :done?   false}

           :toolcall-start
           (let [partial (upsert-tool-call partial {:id        (:id event)
                                                    :name      (:name event)
                                                    :arguments (:arguments event)})]
             {:partial partial
              :signal  {:type :llx.agent.signal/llm-chunk :chunk partial}
              :done?   false})

           :toolcall-delta
           (let [partial (upsert-tool-call partial {:id        (:id event)
                                                    :name      (:name event)
                                                    :arguments (:arguments event)})]
             {:partial partial
              :signal  {:type :llx.agent.signal/llm-chunk :chunk partial}
              :done?   false})

           :toolcall-end
           (let [partial (upsert-tool-call partial {:id        (:id event)
                                                    :name      (:name event)
                                                    :arguments (:arguments event)})]
             {:partial partial
              :signal  {:type :llx.agent.signal/llm-chunk :chunk partial}
              :done?   false})

           :done
           {:partial partial
            :signal  {:type    :llx.agent.signal/llm-done
                      :message (:assistant-message event)}
            :done?   true}

           :error
           {:partial partial
            :signal  {:type  :llx.agent.signal/llm-error
                      :error (:assistant-message event)}
            :done?   true}

           {:partial partial
            :signal  nil
            :done?   false})))

(defn- stream-options
  [{:keys [abort-signal
           session-id
           thinking-budgets
           max-retry-delay-ms]} {:keys [thinking-level]} resolved-api-key]
  (cond-> (merge
           (when-not (= :off thinking-level)
             {:reasoning thinking-level})
           (when abort-signal
             {:signal abort-signal})
           (when session-id
             {:session-id session-id})
           (when thinking-budgets
             {:thinking-budgets thinking-budgets})
           (when max-retry-delay-ms
             {:max-retry-delay-ms max-retry-delay-ms}))
    (some? resolved-api-key)
    (assoc :api-key resolved-api-key)))

(defn fx-call-llm
  [{:keys [state_ convert-to-llm transform-context stream-fn get-api-key abort-signal] :as env} effect]
  (let [out                                           (sp/chan)
        {:keys [model system-prompt tools] :as state} @state_
        default-env-fn                                ai/default-env
        ai-stream-fn                                  ai/stream
        default-stream-fn                             (fn [stream-model context options]
                                                        (ai-stream-fn (default-env-fn) stream-model context options))
        stream-fn                                     (or stream-fn default-stream-fn)
        context                                       (fn [llm-messages]
                                                        {:system-prompt system-prompt
                                                         :messages      llm-messages
                                                         :tools         tools})]
    (-> (p/let [messages         (if transform-context
                                   (transform-context (:messages effect) abort-signal)
                                   (:messages effect))
                llm-messages     (convert-to-llm messages)
                resolved-api-key (if get-api-key
                                   (get-api-key (name (:provider model)))
                                   nil)
                options          (stream-options env state resolved-api-key)
                llm-context      (context llm-messages)
                stream-ch        (stream-fn model llm-context options)]
          (p/loop [partial nil]
            (p/let [event (sp/take stream-ch)]
              (if (nil? event)
                nil
                (let [{:keys [partial signal done?]} (llm-event->step model partial event)]
                  (if signal
                    (p/let [_ (sp/put out signal)]
                      (if done?
                        nil
                        (p/recur partial)))
                    (p/recur partial)))))))
        (p/catch (fn [error]
                   (sp/put out {:type  :llx.agent.signal/llm-error
                                :error error})))
        (p/finally (fn [_ _]
                     (sp/close out))))
    out))
