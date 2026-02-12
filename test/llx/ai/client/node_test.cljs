(ns llx.ai.client.node-test
  (:require
   [clojure.test :refer [deftest is testing async]]
   [llx.ai :as ai]
   [llx.ai.impl.client :as impl.client]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(def base-model
  {:id             "gpt-4o-mini"
   :name           "GPT-4o Mini"
   :provider       :openai
   :api            :openai-completions
   :base-url       "https://api.openai.com/v1"
   :context-window 128000
   :max-tokens     16384
   :cost           {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0}
   :capabilities   {:reasoning? false :input #{:text}}})

(def base-context
  {:messages [{:role :user :content "hello" :timestamp 1}]})

(defn- fail-and-done!
  [done err]
  (is nil (str err))
  (done))

(defn- header-object
  [m]
  #js {:forEach (fn [f]
                  (doseq [[k v] m]
                    (f v k)))})

(defn- response
  [{:keys [status headers text body]}]
  #js {:ok      (and (<= 200 status) (< status 300))
       :status  status
       :headers (header-object (or headers {}))
       :text    (fn [] (js/Promise.resolve (or text "")))
       :body    body})

(defn- body-from-chunks
  [chunks]
  (let [encoder (js/TextEncoder.)
        idx*    (atom 0)]
    #js {:getReader (fn []
                      #js {:read   (fn []
                                     (if (< @idx* (count chunks))
                                       (let [chunk (nth chunks @idx*)]
                                         (swap! idx* inc)
                                         (js/Promise.resolve
                                          #js {:done  false
                                               :value (.encode encoder chunk)}))
                                       (js/Promise.resolve #js {:done true})))
                           :cancel (fn [] (js/Promise.resolve nil))})}))

(defn- blocking-body
  [signal aborted*]
  #js {:getReader (fn []
                    #js {:read   (fn []
                                   (js/Promise.
                                    (fn [resolve _reject]
                                      (.addEventListener signal "abort"
                                                         (fn []
                                                           (resolve (clj->js {:done true})))))))
                         :cancel (fn []
                                   (reset! aborted* true)
                                   (js/Promise.resolve nil))})})

(defn- sleep-ms
  [ms]
  (p/create
   (fn [resolve _reject]
     (js/setTimeout (fn [] (resolve true)) ms))))

(defn- collect-events
  [ch]
  (p/create
   (fn [resolve reject]
     (letfn [(step [events]
               (-> (sp/take ch)
                   (p/then (fn [event]
                             (if (nil? event)
                               (resolve events)
                               (step (conj events event)))))
                   (p/catch reject)))]
       (step [])))))

(defn- with-fetch-stub
  [stub-fn thunk]
  (let [global-fetch (.-fetch js/globalThis)]
    (set! (.-fetch js/globalThis) stub-fn)
    (-> (thunk)
        (p/finally (fn []
                     (set! (.-fetch js/globalThis) global-fetch))))))

(deftest complete-path-uses-node-fetch
  (async done
         (let [env (ai/default-env)]
           (-> (with-fetch-stub (fn [_url _request]
                                  (js/Promise.resolve
                                   (response
                                    {:status 200
                                     :text   "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"role\":\"assistant\",\"content\":\"ok\"}}],\"usage\":{\"prompt_tokens\":1,\"completion_tokens\":1,\"total_tokens\":2}}"})))
                 (fn []
                   (ai/complete* env base-model base-context {:api-key "x"})))
               (p/then (fn [assistant]
                         (is (= :assistant (:role assistant)))
                         (is (= :stop (:stop-reason assistant)))
                         (is (= [{:type :text :text "ok"}] (:content assistant)))
                         (done)))
               (p/catch (partial fail-and-done! done))))))

(deftest stream-path-decodes-sse-across-chunk-boundaries
  (async done
         (let [env (ai/default-env)]
           (-> (with-fetch-stub (fn [_url _request]
                                  (js/Promise.resolve
                                   (response
                                    {:status 200
                                     :body   (body-from-chunks
                                              ["data: {\"choices\":[{\"delta\":{\"content\":\"ok\"}"
                                               ",\"index\":0}]}\n"
                                               "data: {\"choices\":[{\"delta\":{\"content\":\" there\"},\"index\":0,\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":1,\"completion_tokens\":2,\"total_tokens\":3}}\n"
                                               "data: [DONE]\n"])})))
                 (fn []
                   (let [ch (ai/stream* env base-model base-context {:api-key "x"})]
                     (collect-events ch))))
               (p/then (fn [events]
                         (is (= :start (:type (first events))))
                         (is (= :done (:type (last events))))
                         (is (= "ok there"
                                (->> (get-in (last events) [:assistant-message :content])
                                     (filter #(= :text (:type %)))
                                     (map :text)
                                     (apply str))))
                         (done)))
               (p/catch (partial fail-and-done! done))))))

(deftest closing-stream-channel-aborts-upstream-request
  (async done
         (let [env      (ai/default-env)
               aborted* (atom false)]
           (-> (with-fetch-stub
                 (fn [_url request]
                   (let [signal (.-signal request)]
                     (.addEventListener signal "abort"
                                        (fn []
                                          (reset! aborted* true)))
                     (js/Promise.resolve
                      (response
                       {:status 200
                        :body   (blocking-body signal aborted*)}))))
                 (fn []
                   (let [ch (ai/stream* env base-model base-context {:api-key "x"})]
                     (sp/close ch)
                     (-> (sleep-ms 300)
                         (p/then (fn [_]
                                   (is @aborted*)
                                   true))))))
               (p/then (fn [_]
                         (done)))
               (p/catch (partial fail-and-done! done))))))

(deftest stream-failure-emits-terminal-error-event
  (async done
         (let [env (ai/default-env)]
           (-> (with-fetch-stub (fn [_url _request]
                                  (js/Promise.reject (js/Error. "stream boom")))
                 (fn []
                   (let [ch (ai/stream* env base-model base-context {:api-key "x"})]
                     (collect-events ch))))
               (p/then (fn [events]
                         (is (= :error (:type (last events))))
                         (is (= :error
                                (get-in (last events) [:assistant-message :stop-reason])))
                         (done)))
               (p/catch (partial fail-and-done! done))))))

(deftest default-env-exposes-cljs-node-runtime-hooks
  (let [env (ai/default-env)]
    (testing "required env hooks are present"
      (is (fn? (:http/request env)))
      (is (fn? (:json/encode env)))
      (is (fn? (:json/decode env)))
      (is (fn? (:stream/run! env)))
      (is (fn? (:clock/now-ms env)))
      (is (fn? (:id/new env)))
      (is (fn? (:unicode/sanitize-payload env))))
    (testing "json/decode-safe returns nil for invalid JSON"
      (is (nil? ((:json/decode-safe env) "{" {}))))
    (testing "runtime-specific hooks are callable"
      (is (fn? (:http/read-body-string env))))
    (testing "promesa is available in cljs test runtime"
      (is (= 42 @(p/resolved 42))))))

(deftest api-contract-uses-deferred-and-channel-surfaces
  (let [env        {:env :ok}
        model      {:id "m"}
        context    {:messages []}
        complete-d (p/resolved {:role :assistant})
        stream-ch  (sp/chan)]
    (with-redefs [impl.client/complete* (fn [_e _m _c _o] complete-d)
                  impl.client/complete  (fn [_e _m _c _o] complete-d)
                  impl.client/stream*   (fn [_e _m _c _o] stream-ch)
                  impl.client/stream    (fn [_e _m _c _o] stream-ch)]
      (is (p/deferred? (ai/complete* env model context {})))
      (is (p/deferred? (ai/complete env model context {})))
      (is (sp/chan? (ai/stream* env model context {})))
      (is (sp/chan? (ai/stream env model context {}))))))
