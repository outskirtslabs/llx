(ns llx.ai.impl.client.node
  (:require
   [clojure.string :as str]
   [com.fulcrologic.guardrails.malli.core :refer [>defn >defn-]]
   [llx.ai.impl.client :as client]
   [llx.ai.impl.client.event-stream :as stream]
   [llx.ai.impl.errors :as errors]
   [llx.ai.impl.schema :as schema]
   [llx.ai.impl.utils.unicode :as unicode]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

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

(defn- payload->channel!
  [payload-ch payload]
  (-> (sp/put payload-ch (stream/payload-msg payload))
      (p/then (fn [accepted?]
                (if accepted?
                  true
                  (throw (ex-info "Payload channel closed while streaming" {})))))))

(defn- process-lines!
  [{:keys [payload-ch cancelled?]} lines]
  (p/loop [remaining (seq lines)]
    (cond
      (not remaining)
      true

      (cancelled?)
      false

      :else
      (let [line (first remaining)]
        (if-let [payload (client/runtime-data-line->payload line)]
          (p/let [_ (payload->channel! payload-ch payload)]
            (p/recur (next remaining)))
          (p/recur (next remaining)))))))

(>defn- start-node-source!
        [{:keys [response payload-ch cancelled?] :as input}]
        [:llx/runtime-start-source-input => :llx/runtime-start-source-result]
        (schema/assert-valid! :llx/runtime-start-source-input input)
        (let [reader*          (atom nil)
              local-cancelled* (atom false)
              cancelled-now?   (fn []
                                 (or @local-cancelled*
                                     (cancelled?)))]
          (letfn [(cancel-reader! []
                    (when-let [reader @reader*]
                      (.cancel reader)
                      (reset! reader* nil)))
                  (read-loop! [decoder line-buffer]
                    (if (cancelled-now?)
                      (p/resolved nil)
                      (p/let [chunk (.read @reader*)]
                        (if (.-done chunk)
                          (if (seq line-buffer)
                            (process-lines! {:payload-ch payload-ch
                                             :cancelled? cancelled-now?}
                                            [line-buffer])
                            true)
                          (let [chunk-text          (.decode decoder (.-value chunk) #js {:stream true})
                                combined            (str line-buffer chunk-text)
                                {:keys [lines
                                        remainder]} (client/runtime-split-lines combined)]
                            (p/let [ok? (process-lines! {:payload-ch payload-ch
                                                         :cancelled? cancelled-now?}
                                                        lines)]
                              (if ok?
                                (read-loop! decoder remainder)
                                false)))))))]
            (-> (p/resolved nil)
                (p/then (fn [_]
                          (let [stream-body (:body response)]
                            (when-not stream-body
                              (throw (errors/invalid-response
                                      "unknown"
                                      "Stream response missing body"
                                      :context {:status (:status response)})))
                            (let [reader  (.getReader stream-body)
                                  decoder (js/TextDecoder.)]
                              (reset! reader* reader)
                              (read-loop! decoder "")))))
                (p/catch (fn [ex]
                           (if (cancelled-now?)
                             nil
                             (sp/put payload-ch (stream/error-msg ex)))))
                (p/finally (fn [_ _]
                             (cancel-reader!)
                             (sp/close payload-ch)))))
          {:stop-fn (fn []
                      (when (compare-and-set! local-cancelled* false true)
                        (when-let [reader @reader*]
                          (.cancel reader)
                          (reset! reader* nil))
                        (sp/close payload-ch)))}))

(>defn run-stream!
       [{:keys [adapter env model request request-opts] :as input}]
       [:llx/runtime-run-stream-base-input => :llx/runtime-run-stream-result]
       (let [abort-controller   (js/AbortController.)
             request            (assoc request :signal (or (:signal request)
                                                           (.-signal abort-controller)))
             cancel-requested?* (atom false)
             request-started?*  (atom false)
             abort-request!     (fn []
                                  (when-not (.-aborted (.-signal abort-controller))
                                    (.abort abort-controller)))
             cancel-request!    (fn []
                                  (reset! cancel-requested?* true)
                                  (when @request-started?*
                                    (abort-request!)))
             open-stream!       (fn []
                                  (let [max-retries (get request-opts :max-retries 2)
                                        sleep-fn    (or (:thread/sleep env)
                                                        (fn [ms]
                                                          (p/delay (long (max 0 (or ms 0))) nil)))]
                                    (-> (errors/retry-loop-async
                                         (fn []
                                           (reset! request-started?* true)
                                           (let [response-d ((:open-stream adapter) env model request)]
                                             (when @cancel-requested?*
                                               (abort-request!))
                                             response-d))
                                         max-retries
                                         sleep-fn
                                         {:call-id  (:call/id env)
                                          :provider (:provider model)})
                                        (p/then (fn [response]
                                                  (schema/assert-valid! :llx/http-response-map response)))
                                        (p/then (fn [response]
                                                  (if (:body response)
                                                    response
                                                    (throw (errors/invalid-response
                                                            (name (or (:provider model) "unknown"))
                                                            "Stream response missing body"
                                                            :context {:status (:status response)}))))))))]
         (schema/assert-valid!
          :llx/runtime-run-stream-result
          (stream/run-stream!
           (assoc input
                  :request request
                  :cancel! cancel-request!
                  :open-stream! open-stream!
                  :start-source! (fn [args]
                                   (let [base-stop-fn (:stop-fn (start-node-source! args))]
                                     {:stop-fn (fn []
                                                 (cancel-request!)
                                                 (when base-stop-fn
                                                   (base-stop-fn)))})))))))

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
