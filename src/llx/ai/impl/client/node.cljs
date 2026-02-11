(ns llx.ai.impl.client.node
  (:require
   [llx.ai.impl.client :as client]
   [llx.ai.impl.utils.unicode :as unicode]
   [promesa.core :as p]))

(defn- not-implemented
  [feature]
  (throw (ex-info (str "CLJS Node default env does not implement " (name feature)
                       "; provide an env override for this key")
                  {:type    :llx/not-implemented
                   :feature feature})))

(defn- parse-json
  [s]
  (js->clj (.parse js/JSON s) :keywordize-keys true))

(defn- maybe-random-uuid
  []
  (let [crypto (when (exists? js/globalThis) (.-crypto js/globalThis))
        f      (when crypto (.-randomUUID crypto))]
    (when f
      (.call f crypto))))

(defn default-env
  []
  {:http/request             (fn [_request] (not-implemented :http/request))
   :json/encode              (fn [x] (.stringify js/JSON (clj->js x)))
   :json/decode              (fn [s _opts] (parse-json s))
   :json/decode-safe         (fn [s _opts]
                               (try
                                 (parse-json s)
                                 (catch :default _
                                   nil)))
   :http/read-body-string    (fn [body] (if (string? body) body (str body)))
   :stream/run!              (fn [_]
                               ;; Keep runtime behavior async-compatible in CLJS scaffolding.
                               (p/do! (not-implemented :stream/run!)))
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
   :thread/sleep             (fn [_ms] nil)
   :unicode/sanitize-payload unicode/sanitize-payload})
