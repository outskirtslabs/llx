(ns ol.llx.agent.fx.tools
  (:require
   [ol.llx.agent.schema :as schema]
   [ol.llx.ai :as ai]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(defn- now-ms
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

(defn- resolve-tool
  [tools tool-name]
  (some (fn [tool]
          (when (= tool-name (:name tool))
            tool))
        tools))

(defn- ensure-tool-result-content
  [tool-call result]
  (if (and (map? result) (vector? (:content result)))
    (:content result)
    (throw
     (ex-info "Malformed tool result payload"
              {:type         :ol.llx/invalid-tool-result
               :tool-call-id (:id tool-call)
               :tool-name    (:name tool-call)
               :result       result}))))

(defn- success-tool-result-message
  [schema-registry tool-call result]
  (let [content (ensure-tool-result-content tool-call result)]
    (schema/validate! schema-registry
                      :ol.llx/message-tool-result
                      {:role         :tool-result
                       :tool-call-id (:id tool-call)
                       :tool-name    (:name tool-call)
                       :content      content
                       :is-error?    false
                       :timestamp    (now-ms)})))

(defn- error-tool-result-message
  [schema-registry tool-call error]
  (schema/validate! schema-registry
                    :ol.llx/message-tool-result
                    {:role         :tool-result
                     :tool-call-id (:id tool-call)
                     :tool-name    (:name tool-call)
                     :content      [{:type :text
                                     :text (or (ex-message error) (str error))}]
                     :is-error?    true
                     :timestamp    (now-ms)}))

(defn fx-execute-tool
  [{:keys [state_ abort-signal schema-registry]} effect]
  (let [out       (sp/chan)
        tool-call (:tool-call effect)]
    (-> (p/let [tools          (:tools @state_)
                validated-args (ai/validate-tool-call tools tool-call)
                tool-name      (:name tool-call)
                tool           (or (resolve-tool tools tool-name)
                                   (throw
                                    (ex-info "Tool not found in runtime registry"
                                             {:type      :ol.llx/tool-not-found
                                              :tool-name tool-name})))
                execute-fn     (:execute tool)
                on-update      (fn [partial-result]
                                 (sp/put out {:type           :ol.llx.agent.signal/tool-update
                                              :tool-call-id   (:id tool-call)
                                              :tool-name      (:name tool-call)
                                              :partial-result (if (map? partial-result)
                                                                partial-result
                                                                {:value partial-result})}))
                raw-result     (execute-fn (:id tool-call)
                                           validated-args
                                           abort-signal
                                           on-update)
                result-message (success-tool-result-message schema-registry tool-call raw-result)]
          (sp/put out {:type                :ol.llx.agent.signal/tool-result
                       :result              result-message
                       :tool-result-message result-message}))
        (p/catch (fn [error]
                   (let [tool-result-message (error-tool-result-message schema-registry tool-call error)]
                     (sp/put out {:type                :ol.llx.agent.signal/tool-error
                                  :tool-call-id        (:id tool-call)
                                  :error               error
                                  :tool-result-message tool-result-message}))))
        (p/finally (fn [_ _]
                     (sp/close out))))
    out))
