(ns llx.ai.impl.client.node
  (:require
   [clojure.string :as str]
   [com.fulcrologic.guardrails.malli.core :refer [>defn]]
   [llx.ai.impl.client :as client]
   [llx.ai.impl.errors :as errors]
   [llx.ai.impl.schema :as schema]
   [llx.ai.impl.utils.unicode :as unicode]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]
   [taoensso.trove :as trove]))

(defn- parse-json
  [s]
  (js->clj (.parse js/JSON s) :keywordize-keys true))

(defn- maybe-random-uuid
  []
  (let [crypto (when (exists? js/globalThis) (.-crypto js/globalThis))
        f      (when crypto (.-randomUUID crypto))]
    (when f
      (.call f crypto))))

(defn- provider-from-url
  [url]
  (try
    (client/provider-from-host
     (.-hostname (js/URL. (str url))))
    (catch :default _
      "unknown")))

(defn- request->fetch-init
  [request]
  (let [init #js {:method (-> (or (:method request) :get)
                              name
                              str/upper-case)}]
    (when-let [headers (:headers request)]
      (aset init "headers" (clj->js headers)))
    (when (contains? request :body)
      (aset init "body" (:body request)))
    (when-let [signal (:signal request)]
      (aset init "signal" signal))
    init))

(defn- headers->map
  [headers]
  (let [acc (atom {})]
    (when headers
      (.forEach headers
                (fn [value key]
                  (swap! acc assoc
                         (str/lower-case (str key))
                         (str value)))))
    @acc))

(defn- response->map
  [response request]
  (let [status   (or (.-status response) 0)
        headers  (headers->map (.-headers response))
        stream?  (= :stream (:as request))
        success? (and (<= 200 status) (< status 300))]
    (if (and stream? success?)
      {:status status :headers headers :body (.-body response)}
      (-> (.text response)
          (p/then (fn [body]
                    {:status  status
                     :headers headers
                     :body    body}))))))

(defn- wrapped-http-request
  [request]
  (-> (js/fetch (:url request) (request->fetch-init request))
      (p/then (fn [response]
                (response->map response request)))
      (p/catch (fn [ex]
                 (if (and (:signal request) (.-aborted (:signal request)))
                   (throw (ex-info "Request was aborted" {:aborted? true}))
                   (throw (errors/connection-error
                           (provider-from-url (:url request))
                           (or (ex-message ex) (str ex)))))))))

(defn- cancelled?
  [out cancelled*]
  (or @cancelled* (sp/closed? out)))

(defn- emit-events!
  [out cancel-fn events]
  (letfn [(step [remaining]
            (if-not remaining
              (p/resolved true)
              (let [event (first remaining)]
                (schema/assert-valid! :llx/event event)
                (-> (sp/put out event)
                    (p/then (fn [accepted?]
                              (if accepted?
                                (step (next remaining))
                                (do
                                  (cancel-fn)
                                  false))))))))]
    (step (seq events))))

(defn- emit-terminal-error!
  [adapter env model out state* stream-ex]
  (sp/put out (client/runtime-terminal-error-event adapter env model state* stream-ex)))

(defn- process-line!
  [adapter env model out state* cancel-fn item-index* line]
  (if-let [payload (client/runtime-data-line->payload line)]
    (try
      (let [provider-item-type     (client/runtime-payload->provider-item-type env payload)
            {:keys [state events]} (schema/assert-valid!
                                    :llx/runtime-decode-event-result
                                    ((:decode-event adapter) env @state* payload))
            event-type             (:type (first events))
            payload-bytes          (count payload)
            idx                    @item-index*]
        (swap! item-index* inc)
        (trove/log! {:level :trace
                     :id    :llx.obs/stream-item-received
                     :data  {:call-id            (:call/id env)
                             :provider           (:provider model)
                             :api                (:api model)
                             :model-id           (:id model)
                             :item-index         idx
                             :provider-item-type provider-item-type
                             :llx-event-type     event-type
                             :done?              false
                             :payload-bytes      payload-bytes}})
        (reset! state* state)
        (emit-events! out cancel-fn events))
      (catch :default ex
        (p/rejected ex)))
    (p/resolved true)))

(defn- process-lines!
  [adapter env model out state* cancel-fn item-index* cancelled* lines]
  (letfn [(step [remaining]
            (if-not (seq remaining)
              (p/resolved true)
              (if (cancelled? out cancelled*)
                (p/resolved false)
                (-> (process-line! adapter env model out state* cancel-fn item-index* (first remaining))
                    (p/then (fn [ok?]
                              (if ok?
                                (step (next remaining))
                                false)))))))]
    (step lines)))

(defn- stream-events!
  [adapter env model out state* cancel-fn cancelled* stream-body]
  (let [reader       (.getReader stream-body)
        decoder      (js/TextDecoder.)
        line-buffer* (atom "")
        item-index*  (atom 0)]
    (letfn [(step []
              (if (cancelled? out cancelled*)
                (p/resolved false)
                (-> (.read reader)
                    (p/then (fn [chunk]
                              (if (.-done chunk)
                                (if (seq @line-buffer*)
                                  (-> (process-lines! adapter env model out state* cancel-fn item-index* cancelled* [@line-buffer*])
                                      (p/then (fn [ok?]
                                                (when ok?
                                                  (reset! line-buffer* ""))
                                                ok?)))
                                  (p/resolved true))
                                (let [chunk-text          (.decode decoder (.-value chunk) #js {:stream true})
                                      combined            (str @line-buffer* chunk-text)
                                      {:keys [lines
                                              remainder]} (client/runtime-split-lines combined)]
                                  (reset! line-buffer* remainder)
                                  (-> (process-lines! adapter env model out state* cancel-fn item-index* cancelled* lines)
                                      (p/then (fn [ok?]
                                                (if ok?
                                                  (step)
                                                  false)))))))))))]
      (step))))

(>defn run-stream!
       [{:keys [adapter env model request out state* request-opts]}]
       [:llx/runtime-run-stream-input => any?]
       (schema/assert-valid! :llx/runtime-run-stream-input
                             {:adapter      adapter
                              :env          env
                              :model        model
                              :request      request
                              :out          out
                              :state*       state*
                              :request-opts request-opts})
       (let [cancelled*       (atom false)
             done*            (atom false)
             abort-controller (js/AbortController.)
             request          (assoc request :signal (or (:signal request)
                                                         (.-signal abort-controller)))]
         (letfn [(cancel-fn []
                   (when (compare-and-set! cancelled* false true)
                     (when-not (.-aborted (.-signal abort-controller))
                       (.abort abort-controller))))
                 (finalize-stream! []
                   (let [{:keys [assistant-message events]} (schema/assert-valid!
                                                             :llx/runtime-finalize-result
                                                             ((:finalize adapter) env @state*))]
                     (-> (emit-events! out cancel-fn events)
                         (p/then (fn [events-ok?]
                                   (if-not events-ok?
                                     false
                                     (do
                                       (schema/assert-valid! :llx/message-assistant assistant-message)
                                       (trove/log! {:level :info
                                                    :id    :llx.obs/stream-done
                                                    :data  {:call-id             (:call/id env)
                                                            :provider            (:provider model)
                                                            :api                 (:api model)
                                                            :model-id            (:id model)
                                                            :stop-reason         (:stop-reason assistant-message)
                                                            :usage               (:usage assistant-message)
                                                            :content-block-count (count (:content assistant-message))}})
                                       (sp/put out {:type              :done
                                                    :assistant-message assistant-message}))))))))]
           (-> (errors/retry-loop-async
                #((:open-stream adapter) env model request)
                (get request-opts :max-retries 2)
                (or (:thread/sleep env) (fn [_ms] nil))
                {:call-id  (:call/id env)
                 :provider (:provider model)})
               (p/then (fn [response]
                         (let [response (schema/assert-valid! :llx/http-response-map response)]
                           (trove/log! {:level :info
                                        :id    :llx.obs/stream-start
                                        :data  {:call-id  (:call/id env)
                                                :provider (:provider model)
                                                :api      (:api model)
                                                :model-id (:id model)}})
                           (if-not (:body response)
                             (p/rejected (errors/invalid-response
                                          (name (or (:provider model) "unknown"))
                                          "Stream response missing body"
                                          :context {:status (:status response)}))
                             (-> (sp/put out {:type :start})
                                 (p/then (fn [accepted?]
                                           (if-not accepted?
                                             (do
                                               (cancel-fn)
                                               false)
                                             (stream-events! adapter env model out state* cancel-fn cancelled* (:body response)))))
                                 (p/then (fn [stream-ok?]
                                           (if (or (not stream-ok?)
                                                   (cancelled? out cancelled*))
                                             false
                                             (finalize-stream!)))))))))
               (p/catch (fn [stream-ex]
                          (if (cancelled? out cancelled*)
                            nil
                            (emit-terminal-error! adapter env model out state* stream-ex))))
               (p/finally (fn []
                            (reset! done* true)
                            (sp/close out))))
           {:cancel-fn cancel-fn
            :done?     (fn [] (boolean @done*))})))

(defn default-env
  []
  {:http/request             wrapped-http-request
   :json/encode              (fn [x] (.stringify js/JSON (clj->js x)))
   :json/decode              (fn [s _opts] (parse-json s))
   :json/decode-safe         (fn [s _opts]
                               (try
                                 (parse-json s)
                                 (catch :default _
                                   nil)))
   :http/read-body-string    (fn [body] (if (string? body) body (str body)))
   :stream/run!              run-stream!
   :registry                 client/default-registry
   :clock/now-ms             (fn [] (.now js/Date))
   :id/new                   (fn []
                               (or (maybe-random-uuid)
                                   (str "llx-" (.now js/Date) "-" (rand-int 1000000))))
   :env/get                  (fn [k]
                               (let [process (when (exists? js/process) js/process)
                                     env     (when process (.-env process))]
                                 (when env
                                   (aget env (name k)))))
   :thread/sleep             (fn [ms]
                               (p/create (fn [resolve _reject]
                                           (js/setTimeout (fn [] (resolve nil))
                                                          (long (max 0 (or ms 0)))))))
   :unicode/sanitize-payload unicode/sanitize-payload})
