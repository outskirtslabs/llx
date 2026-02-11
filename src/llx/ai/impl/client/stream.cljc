(ns llx.ai.impl.client.stream
  (:require
   #?@(:clj [[clojure.main :as main]])
   [llx.ai.impl.client :as client]
   [llx.ai.impl.errors :as errors]
   [llx.ai.impl.schema :as schema]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]
   [taoensso.trove :as trove]))

(defn- root-cause
  [t]
  #?(:clj (main/root-cause t)
     :cljs t))

(defn await!
  ([x]
   (if (p/deferred? x)
     (let [result (p/await x)]
       (if (instance? #?(:clj Throwable :cljs js/Error) result)
         (throw (root-cause result))
         result))
     x))
  ([x duration default-on-timeout]
   (if (p/deferred? x)
     (let [result (p/await x duration default-on-timeout)]
       (if (instance? #?(:clj Throwable :cljs js/Error) result)
         (throw (root-cause result))
         result))
     x)))

(defn validate-run-stream-input!
  [{:keys [adapter env model request out state* request-opts] :as input}]
  (schema/assert-valid! :llx/runtime-run-stream-input
                        {:adapter      adapter
                         :env          env
                         :model        model
                         :request      request
                         :out          out
                         :state*       state*
                         :request-opts request-opts})
  input)

(defn- default-sleep
  [ms]
  (p/delay (long (max 0 (or ms 0))) nil))

(defn open-stream-with-retries*
  [{:keys [adapter env model request request-opts]}]
  (let [max-retries (get request-opts :max-retries 2)
        sleep-fn    (or (:thread/sleep env) default-sleep)]
    (-> (errors/retry-loop-async
         (fn []
           ((:open-stream adapter) env model request))
         max-retries
         sleep-fn
         {:call-id  (:call/id env)
          :provider (:provider model)})
        (p/then (fn [response]
                  (schema/assert-valid! :llx/http-response-map response))))))

(defn emit-events*
  [{:keys [emit! cancel-fn events]}]
  (letfn [(step [remaining]
            (if-not remaining
              (p/resolved true)
              (let [event (first remaining)]
                (schema/assert-valid! :llx/event event)
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
  (trove/log! {:level :info
               :id    :llx.obs/stream-start
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
                                :llx/runtime-decode-event-result
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
                 :id    :llx.obs/stream-item-received
                 :data  item-log-data})
    (reset! state* next-state)
    (-> (emit-events* {:emit! emit! :cancel-fn cancel-fn :events events})
        (p/then (fn [ok?]
                  {:ok?             (boolean ok?)
                   :next-item-index next-item-index})))))

(defn finalize-step
  [{:keys [adapter env state]}]
  (let [{:keys [assistant-message events]} (schema/assert-valid!
                                            :llx/runtime-finalize-result
                                            ((:finalize adapter) env state))
        assistant-message                  (schema/assert-valid! :llx/message-assistant assistant-message)]
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
                      (trove/log! {:level :info
                                   :id    :llx.obs/stream-done
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

(defn eof-msg
  []
  {:type :eof})

(defn error-msg
  [ex]
  {:type :error :error ex})

(def ^:private runtime-stream-hooks-schema
  [:map {:closed true}
   [:start-source! :llx/fn]
   [:open-stream! :llx/fn]])

(defn- emit!
  [out event]
  (-> (sp/put out event)
      (p/then boolean)))

(defn- stop-source!
  [source-stop-fn*]
  (when-let [stop-fn @source-stop-fn*]
    (reset! source-stop-fn* nil)
    (stop-fn)))

(defn run-stream!
  [{:keys [adapter env model request out state* start-source! open-stream!]
    :as   args}]
  (schema/assert-valid! :llx/runtime-run-stream-input args)
  (schema/assert-valid! runtime-stream-hooks-schema
                        {:start-source! start-source!
                         :open-stream!  open-stream!})
  (let [cancelled*      (atom false)
        done*           (atom false)
        source-stop-fn* (atom nil)
        payload-ch      (sp/chan)
        control-ch      (sp/chan :buf (sp/sliding-buffer 1))]
    (letfn [(cancelled-now? []
              (or @cancelled* (sp/closed? out)))
            (cancel-fn []
              (when (compare-and-set! cancelled* false true)
                (stop-source! source-stop-fn*)
                (sp/close control-ch)
                true))
            (emit-event! [event]
              (emit! out event))
            (process-payload! [item-index payload]
              (process-payload-step* {:adapter   adapter
                                      :env       env
                                      :model     model
                                      :state*    state*
                                      :emit!     emit-event!
                                      :cancel-fn cancel-fn}
                                     item-index
                                     payload))
            (finalize! []
              (finalize-stream* {:adapter   adapter
                                 :env       env
                                 :model     model
                                 :state*    state*
                                 :emit!     emit-event!
                                 :cancel-fn cancel-fn}))
            (emit-terminal-error! [stream-ex]
              (emit-terminal-error* {:adapter adapter
                                     :env     env
                                     :model   model
                                     :state*  state*
                                     :emit!   emit-event!}
                                    stream-ex))
            (consume-stream! []
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
                        (finalize!))

                      (= :error (:type msg))
                      (p/rejected (:error msg))

                      (= :payload (:type msg))
                      (p/let [{:keys [ok? next-item-index]}
                              (process-payload! item-index (:payload msg))]
                        (if ok?
                          (p/recur next-item-index)
                          nil))

                      :else
                      (p/recur item-index))))))
            (run-worker! []
              (if (cancelled-now?)
                (p/resolved nil)
                (p/let [response    (open-stream!)
                        response    (schema/assert-valid! :llx/http-response-map response)
                        start-ok?   (if (cancelled-now?)
                                      false
                                      (emit-start* {:emit!     emit-event!
                                                    :cancel-fn cancel-fn
                                                    :env       env
                                                    :model     model}))
                        source-stop (if (or (not start-ok?) (cancelled-now?))
                                      nil
                                      (start-source! {:adapter    adapter
                                                      :env        env
                                                      :model      model
                                                      :request    request
                                                      :response   response
                                                      :payload-ch payload-ch
                                                      :control-ch control-ch
                                                      :cancelled? cancelled-now?}))]
                  (when-let [stop-fn (:stop-fn source-stop)]
                    (if (cancelled-now?)
                      (stop-fn)
                      (reset! source-stop-fn* stop-fn)))
                  (if (or (not start-ok?) (cancelled-now?))
                    nil
                    (consume-stream!)))))]
      (p/future
        (-> (run-worker!)
            (p/catch (fn [stream-ex]
                       (if (cancelled-now?)
                         nil
                         (emit-terminal-error! stream-ex))))
            (p/finally (fn []
                         (stop-source! source-stop-fn*)
                         (reset! done* true)
                         (sp/close payload-ch)
                         (sp/close control-ch)
                         (sp/close out)))))
      {:cancel-fn  cancel-fn
       :done?      (fn [] (boolean @done*))
       :payload-ch payload-ch
       :control-ch control-ch})))
