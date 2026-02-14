(ns llx.agent.proxy
  "Transport adapter for proxy-backed streaming in `llx.agent`.

  A proxy stream adapter is the boundary between transport-specific event
  payloads and the canonical stream event shape consumed by agent code. This
  namespace consumes `:llx.agent/proxy-event` messages, reconstructs partial
  assistant state (text, thinking, and tool calls), and emits standard LLX
  stream events such as `:start`, `:text-*`, `:toolcall-*`, and terminal
  `:done`/`:error`.

  Why this exists:
  - keeps `llx.agent.loop` and runtime logic transport-agnostic
  - centralizes proxy event normalization in one place
  - makes JVM/CLJS behavior consistent while allowing custom fetch transport
    via `:fetch-stream!`."
  (:require
   #?@(:clj [[babashka.json :as json]])
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [llx.agent.schema :as agent-schema]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(defn- now-ms
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

(defn- json-read-safe
  [raw]
  #?(:clj (try
            (json/read-str raw {:key-fn keyword})
            (catch Exception _
              nil))
     :cljs (try
             (js->clj (.parse js/JSON raw) :keywordize-keys true)
             (catch :default _
               nil))))

(defn- empty-usage
  []
  {:input        0
   :output       0
   :cache-read   0
   :cache-write  0
   :total-tokens 0
   :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}})

(defn- assistant-skeleton
  [model]
  {:role        :assistant
   :content     []
   :api         (:api model)
   :provider    (:provider model)
   :model       (:id model)
   :usage       (empty-usage)
   :stop-reason :stop
   :timestamp   (now-ms)})

(defn- set-content-block
  [content index block]
  (cond
    (= index (count content))
    (conj content block)

    (< index (count content))
    (assoc content index block)

    :else
    (conj content block)))

(defn- update-content-block
  [content index update-fn]
  (if (< index (count content))
    (update content index update-fn)
    content))

(defn- process-proxy-event
  [state proxy-event]
  (let [proxy-event (agent-schema/validate! :llx.agent/proxy-event proxy-event)
        partial     (:partial state)]
    (case (:llx.agent.proxy-event/type proxy-event)
      :start
      {:state  state
       :events [{:type :start}]
       :done?  false}

      :text-start
      (let [index   (:llx.agent.proxy-event/content-index proxy-event)
            partial (update partial
                            :content
                            set-content-block
                            index
                            {:type :text :text ""})]
        {:state  (assoc state :partial partial)
         :events [{:type :text-start}]
         :done?  false})

      :text-delta
      (let [index   (:llx.agent.proxy-event/content-index proxy-event)
            delta   (:llx.agent.proxy-event/delta proxy-event)
            partial (update partial
                            :content
                            update-content-block
                            index
                            (fn [block]
                              (if (= :text (:type block))
                                (update block :text str delta)
                                block)))]
        {:state  (assoc state :partial partial)
         :events [{:type :text-delta :text delta}]
         :done?  false})

      :text-end
      {:state  state
       :events [{:type :text-end}]
       :done?  false}

      :thinking-start
      (let [index   (:llx.agent.proxy-event/content-index proxy-event)
            partial (update partial
                            :content
                            set-content-block
                            index
                            {:type :thinking :thinking ""})]
        {:state  (assoc state :partial partial)
         :events [{:type :thinking-start}]
         :done?  false})

      :thinking-delta
      (let [index   (:llx.agent.proxy-event/content-index proxy-event)
            delta   (:llx.agent.proxy-event/delta proxy-event)
            partial (update partial
                            :content
                            update-content-block
                            index
                            (fn [block]
                              (if (= :thinking (:type block))
                                (update block :thinking str delta)
                                block)))]
        {:state  (assoc state :partial partial)
         :events [{:type :thinking-delta :thinking delta}]
         :done?  false})

      :thinking-end
      {:state  state
       :events [{:type :thinking-end}]
       :done?  false}

      :toolcall-start
      (let [index   (:llx.agent.proxy-event/content-index proxy-event)
            id      (:llx.agent.proxy-event/id proxy-event)
            name    (:llx.agent.proxy-event/tool-name proxy-event)
            partial (update partial
                            :content
                            set-content-block
                            index
                            {:type      :tool-call
                             :id        id
                             :name      name
                             :arguments {}})]
        {:state  (-> state
                     (assoc :partial partial)
                     (assoc-in [:toolcall-raw index] ""))
         :events [{:type :toolcall-start :id id :name name}]
         :done?  false})

      :toolcall-delta
      (let [index       (:llx.agent.proxy-event/content-index proxy-event)
            delta       (:llx.agent.proxy-event/delta proxy-event)
            id          (get-in partial [:content index :id])
            name        (get-in partial [:content index :name])
            raw         (str (get-in state [:toolcall-raw index] "") delta)
            parsed-args (or (json-read-safe raw)
                            (get-in partial [:content index :arguments])
                            {})
            partial     (update partial
                                :content
                                update-content-block
                                index
                                (fn [block]
                                  (if (= :tool-call (:type block))
                                    (assoc block :arguments parsed-args)
                                    block)))]
        {:state  (-> state
                     (assoc :partial partial)
                     (assoc-in [:toolcall-raw index] raw))
         :events [{:type      :toolcall-delta
                   :id        id
                   :name      name
                   :arguments parsed-args}]
         :done?  false})

      :toolcall-end
      (let [index     (:llx.agent.proxy-event/content-index proxy-event)
            block     (get-in partial [:content index])
            id        (:id block)
            name      (:name block)
            arguments (:arguments block)]
        {:state  state
         :events [{:type      :toolcall-end
                   :id        id
                   :name      name
                   :arguments arguments}]
         :done?  false})

      :done
      (let [assistant (assoc partial
                             :stop-reason (:llx.agent.proxy-event/reason proxy-event)
                             :usage (:llx.agent.proxy-event/usage proxy-event))]
        {:state  (assoc state :partial assistant)
         :events [{:type              :done
                   :assistant-message assistant}]
         :done?  true})

      :error
      (let [assistant (assoc partial
                             :stop-reason (:llx.agent.proxy-event/reason proxy-event)
                             :error-message (:llx.agent.proxy-event/error-message proxy-event)
                             :usage (:llx.agent.proxy-event/usage proxy-event))]
        {:state  (assoc state :partial assistant)
         :events [{:type              :error
                   :assistant-message assistant}]
         :done?  true}))))

(defn- emit-events!
  [out events]
  (reduce
   (fn [acc event]
     (-> acc
         (p/then (fn [_]
                   (sp/put out event)))))
   (p/resolved true)
   events))

(>defn stream-proxy
       "Streams LLX events from compact proxy event payloads.

  `:fetch-stream!` may be supplied in `proxy-options` to inject transport.
  It must return a channel of `:llx.agent/proxy-event` maps."
       [env model context proxy-options]
       [:map :llx/model :llx/context-map :llx.agent/proxy-options => :llx.agent/channel]
       (let [proxy-options (agent-schema/validate! :llx.agent/proxy-options proxy-options)
             fetch-stream! (:fetch-stream! proxy-options)
             out           (sp/chan 64)
             initial-state {:partial      (assistant-skeleton model)
                            :toolcall-raw {}}]
         (if-not (fn? fetch-stream!)
           (do
             (sp/close out)
             (throw (ex-info "Proxy options must include :fetch-stream! transport hook."
                             {:type :llx.agent.proxy/missing-fetch-stream})))
           (do
             (-> (p/resolved (fetch-stream! {:env     env
                                             :model   model
                                             :context context
                                             :options (dissoc proxy-options :fetch-stream!)}))
                 (p/then (fn [proxy-ch]
                           (letfn [(consume [state]
                                     (p/let [proxy-event (sp/take proxy-ch)]
                                       (if (nil? proxy-event)
                                         nil
                                         (let [{:keys [state events done?]}
                                               (process-proxy-event state proxy-event)]
                                           (-> (emit-events! out events)
                                               (p/then (fn [_]
                                                         (if done?
                                                           nil
                                                           (consume state)))))))))]
                             (consume initial-state))))
                 (p/catch (fn [error]
                            (let [assistant-message
                                  (assoc (assistant-skeleton model)
                                         :stop-reason :error
                                         :error-message (or (ex-message error)
                                                            "Proxy stream failed."))]
                              (emit-events! out [{:type              :error
                                                  :assistant-message assistant-message}]))))
                 (p/finally (fn [_ _]
                              (sp/close out))))
             out))))
