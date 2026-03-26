(ns ol.llx.ai.impl.client.event-stream
  "Shared stream runtime orchestration for all hosts.

  ## Why

  Host runtimes should differ only in transport concerns (how bytes arrive, how
  cancellation reaches the underlying request). Event decoding, lifecycle
  emission, finalize behavior, and terminal error handling must stay identical.

  ## What

  This namespace owns the shared stream loop:
  - opens a provider stream with retry policy
  - emits canonical `:start` / delta / `:done` events
  - converts runtime failures into canonical terminal error events
  - coordinates cancellation across producer/source and consumer/output channel

  ## How

  Hosts inject two hooks:
  - `:open-stream!` to acquire the transport stream response
  - `:start-source!` to push provider payloads onto `payload-ch`

  The shared loop consumes payload/control channels with Promesa CSP, applies
  adapter decode/finalize functions, and emits canonical events to `out`.

  ## Related Namespaces

  - [[ol.llx.ai.impl.client.jvm]]
  - [[ol.llx.ai.impl.client.node]]
  - [[ol.llx.ai.impl.client]]"
  (:require
   #?@(:clj [[clojure.main :as main]])
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [ol.llx.ai.impl.client :as client]
   [ol.llx.ai.impl.errors :as errors]
   [ol.llx.ai.impl.schema :as schema]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]
   [taoensso.trove :as trove]))

#?(:clj
   (defn- root-cause
     [t]
     (main/root-cause t)))

#?(:clj
   (defn await!
     ([x]
      (if (p/deferred? x)
        (let [result (p/await x)]
          (if (instance? Throwable result)
            (throw (root-cause result))
            result))
        x))
     ([x duration default-on-timeout]
      (if (p/deferred? x)
        (let [result (p/await x duration default-on-timeout)]
          (if (instance? Throwable result)
            (throw (root-cause result))
            result))
        x))))

#?(:cljs
   (defn await!
     ([x]
      x)
     ([x _duration default-on-timeout]
      (if (p/deferred? x)
        default-on-timeout
        x))))

(>defn open-stream-with-retries*
       [{:keys [adapter env model request request-opts]}]
       [[:map
         [:adapter :ol.llx/adapter]
         [:env :ol.llx/env]
         [:model :ol.llx/model]
         [:request :ol.llx/adapter-request-map]
         [:request-opts {:optional true} [:maybe :ol.llx/provider-request-options]]]
        => :ol.llx/deferred]
       (let [max-retries (get request-opts :max-retries 2)]
         (-> (errors/retry-loop-async
              (fn []
                ((:open-stream adapter) env model request))
              max-retries
              p/delay
              {:call-id            (:call/id env)
               :provider           (:provider model)
               :max-retry-delay-ms (:max-retry-delay-ms request-opts)})
             (p/then (fn [response]
                       (schema/assert-valid! :ol.llx/http-response-map response))))))

(defn emit-events*
  [{:keys [emit! cancel-fn events]}]
  (letfn [(step [remaining]
            (if-not remaining
              (p/resolved true)
              (let [event (first remaining)]
                (schema/assert-valid! :ol.llx/event event)
                (-> (emit! event)
                    (p/then (fn [accepted?]
                              (if accepted?
                                (step (next remaining))
                                (do
                                  (cancel-fn)
                                  false))))))))]
    (step (seq events))))

(defn emit-start*
  [{:keys [emit! cancel-fn env model]}]
  (trove/log! {:level :debug
               :id    :ol.llx.obs/stream-start
               :data  {:call-id  (:call/id env)
                       :provider (:provider model)
                       :api      (:api model)
                       :model-id (:id model)}})
  (-> (emit! {:type :start})
      (p/then (fn [accepted?]
                (if accepted?
                  true
                  (do
                    (cancel-fn)
                    false))))))

(defn decode-payload-step
  [{:keys [adapter env model state item-index payload]}]
  (let [provider-item-type     (client/runtime-payload->provider-item-type env payload)
        {:keys [state events]} (schema/assert-valid!
                                :ol.llx/runtime-decode-event-result
                                ((:decode-event adapter) env state payload))
        event-type             (:type (first events))
        payload-bytes          (count payload)]
    {:next-state      state
     :events          events
     :next-item-index (inc item-index)
     :item-log-data   {:call-id            (:call/id env)
                       :provider           (:provider model)
                       :api                (:api model)
                       :model-id           (:id model)
                       :item-index         item-index
                       :provider-item-type provider-item-type
                       :llx-event-type     event-type
                       :done?              false
                       :payload-bytes      payload-bytes}}))

(defn process-payload-step*
  [{:keys [adapter env model state* emit! cancel-fn]} item-index payload]
  (let [{:keys [next-state events next-item-index item-log-data]}
        (decode-payload-step {:adapter    adapter
                              :env        env
                              :model      model
                              :state      @state*
                              :item-index item-index
                              :payload    payload})]
    (trove/log! {:level :trace
                 :id    :ol.llx.obs/stream-item-received
                 :data  item-log-data})
    (reset! state* next-state)
    (-> (emit-events* {:emit! emit! :cancel-fn cancel-fn :events events})
        (p/then (fn [ok?]
                  {:ok?             (boolean ok?)
                   :next-item-index next-item-index})))))

(defn finalize-step
  [{:keys [adapter env state]}]
  (let [{:keys [assistant-message events]} (schema/assert-valid!
                                            :ol.llx/runtime-finalize-result
                                            ((:finalize adapter) env state))
        assistant-message                  (schema/assert-valid! :ol.llx/message-assistant assistant-message)]
    {:assistant-message assistant-message
     :events            events}))

(defn finalize-stream*
  [{:keys [adapter env model state* emit! cancel-fn]}]
  (let [{:keys [assistant-message events]}
        (finalize-step {:adapter adapter
                        :env     env
                        :state   @state*})]
    (-> (emit-events* {:emit! emit! :cancel-fn cancel-fn :events events})
        (p/then (fn [events-ok?]
                  (if-not events-ok?
                    false
                    (do
                      (trove/log! {:level :debug
                                   :id    :ol.llx.obs/stream-done
                                   :data  {:call-id             (:call/id env)
                                           :provider            (:provider model)
                                           :api                 (:api model)
                                           :model-id            (:id model)
                                           :stop-reason         (:stop-reason assistant-message)
                                           :usage               (:usage assistant-message)
                                           :content-block-count (count (:content assistant-message))}})
                      (emit! {:type :done :assistant-message assistant-message}))))))))

(defn emit-terminal-error*
  [{:keys [adapter env model state* emit!]} stream-ex]
  (-> (emit! (client/runtime-terminal-error-event adapter env model state* stream-ex))
      (p/then boolean)))

(defn payload-msg
  [payload]
  {:type :payload :payload payload})

(defn error-msg
  [ex]
  {:type :error :error ex})

(defn- emit!
  [out event]
  (-> (sp/put out event)
      (p/then boolean)))

(defn- stop-source!
  [source-stop-fn*]
  (when-let [stop-fn @source-stop-fn*]
    (reset! source-stop-fn* nil)
    (stop-fn)))

(>defn run-stream!
       [{:keys [adapter env model request out state* cancel! start-source! open-stream!]
         :as   args}]
       [:ol.llx/runtime-run-stream-input => :ol.llx/runtime-run-stream-result]
       (schema/assert-valid! :ol.llx/runtime-run-stream-input args)
       (let [cancelled*      (atom false)
             done*           (atom false)
             source-stop-fn* (atom nil)
             payload-ch      (sp/chan)
             control-ch      (sp/chan :buf (sp/sliding-buffer 1))
             cancelled-now?  (fn []
                               (or @cancelled* (sp/closed? out)))
             cancel-fn       (fn []
                               (when cancel!
                                 (cancel!))
                               (when (compare-and-set! cancelled* false true)
                                 (stop-source! source-stop-fn*)
                                 (sp/close control-ch)
                                 true))
             emit-event!     (fn [event]
                               (emit! out event))
             stream-step-ctx {:adapter   adapter
                              :env       env
                              :model     model
                              :state*    state*
                              :emit!     emit-event!
                              :cancel-fn cancel-fn}
             consume-stream! (fn []
                               (p/loop [item-index 0]
                                 (if (cancelled-now?)
                                   nil
                                   (p/let [[msg ch] (sp/alts [payload-ch control-ch])]
                                     (cond
                                       (= ch control-ch)
                                       nil

                                       (or (nil? msg)
                                           (= :eof (:type msg)))
                                       (if (cancelled-now?)
                                         nil
                                         (finalize-stream* stream-step-ctx))

                                       (= :error (:type msg))
                                       (p/rejected (:error msg))

                                       (= :payload (:type msg))
                                       (p/let [{:keys [ok? next-item-index]}
                                               (process-payload-step* stream-step-ctx
                                                                      item-index
                                                                      (:payload msg))]
                                         (if ok?
                                           (p/recur next-item-index)
                                           nil))

                                       :else
                                       (p/recur item-index))))))
             run-worker!     (fn []
                               (p/let [response                 (open-stream!)
                                       response                 (schema/assert-valid! :ol.llx/http-response-map response)
                                       cancelled-after-open?    (cancelled-now?)
                                       _                        (when cancelled-after-open?
                                                                  (cancel-fn))
                                       start-ok?                (if cancelled-after-open?
                                                                  false
                                                                  (emit-start* {:emit!     emit-event!
                                                                                :cancel-fn cancel-fn
                                                                                :env       env
                                                                                :model     model}))
                                       cancelled-before-source? (cancelled-now?)
                                       _                        (when cancelled-before-source?
                                                                  (cancel-fn))
                                       source-input             (if (or (not start-ok?) cancelled-before-source?)
                                                                  nil
                                                                  {:adapter    adapter
                                                                   :env        env
                                                                   :model      model
                                                                   :request    request
                                                                   :response   response
                                                                   :payload-ch payload-ch
                                                                   :control-ch control-ch
                                                                   :cancelled? cancelled-now?})
                                       source-stop              (when source-input
                                                                  (let [validated-source-input
                                                                        (schema/assert-valid! :ol.llx/runtime-start-source-input
                                                                                              source-input)]
                                                                    (schema/assert-valid! :ol.llx/runtime-start-source-result
                                                                                          (start-source! validated-source-input))))]
                                 (when-let [stop-fn (:stop-fn source-stop)]
                                   (if (cancelled-now?)
                                     (stop-fn)
                                     (reset! source-stop-fn* stop-fn)))
                                 (if (or (not start-ok?) (cancelled-now?))
                                   nil
                                   (consume-stream!))))]
         (-> (run-worker!)
             (p/catch (fn [stream-ex]
                        (if (cancelled-now?)
                          nil
                          (emit-terminal-error* stream-step-ctx stream-ex))))
             (p/finally (fn [_ _]
                          (stop-source! source-stop-fn*)
                          (reset! done* true)
                          (sp/close payload-ch)
                          (sp/close control-ch)
                          (sp/close out))))
         (schema/assert-valid!
          :ol.llx/runtime-run-stream-result
          {:cancel-fn  cancel-fn
           :done?      (fn [] (boolean @done*))
           :payload-ch payload-ch
           :control-ch control-ch})))
