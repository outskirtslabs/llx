(ns llx.agent.proxy-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]
             [llx.ai.test-util :as util]]
       :cljs [[cljs.test :refer-macros [deftest is]]
              [llx.ai.test-util :as util :include-macros true]])
   [llx.agent.proxy :as sut]
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

(def base-usage
  {:input        1
   :output       1
   :cache-read   0
   :cache-write  0
   :total-tokens 2
   :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}})

(defn- channel-from-events
  [events]
  (let [ch (sp/chan 32)]
    (-> (reduce
         (fn [acc event]
           (p/then acc
                   (fn [_]
                     (sp/put ch event))))
         (p/resolved true)
         events)
        (p/finally (fn [_ _]
                     (sp/close ch))))
    ch))

(deftest stream-proxy-reconstructs-partial-and-emits-done
  (util/async done
              (let [proxy-events [{:llx.agent.proxy-event/type :start}
                                  {:llx.agent.proxy-event/type          :text-start
                                   :llx.agent.proxy-event/content-index 0}
                                  {:llx.agent.proxy-event/type          :text-delta
                                   :llx.agent.proxy-event/content-index 0
                                   :llx.agent.proxy-event/delta         "he"}
                                  {:llx.agent.proxy-event/type          :text-delta
                                   :llx.agent.proxy-event/content-index 0
                                   :llx.agent.proxy-event/delta         "llo"}
                                  {:llx.agent.proxy-event/type          :text-end
                                   :llx.agent.proxy-event/content-index 0}
                                  {:llx.agent.proxy-event/type   :done
                                   :llx.agent.proxy-event/reason :stop
                                   :llx.agent.proxy-event/usage  base-usage}]
                    stream-ch    (sut/stream-proxy
                                  {}
                                  base-model
                                  {:system-prompt ""
                                   :messages      []}
                                  {:proxy-url     "https://proxy.example"
                                   :auth-token    "token"
                                   :fetch-stream! (fn [_request]
                                                    (channel-from-events proxy-events))})]
                (-> (util/collect-events* stream-ch 2000)
                    (p/then (fn [events]
                              (is (= :start (:type (first events))))
                              (is (= :done (:type (last events))))
                              (is (= "hello"
                                     (get-in (last events)
                                             [:assistant-message :content 0 :text])))))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest stream-proxy-emits-terminal-error
  (util/async done
              (let [proxy-events [{:llx.agent.proxy-event/type :start}
                                  {:llx.agent.proxy-event/type          :text-start
                                   :llx.agent.proxy-event/content-index 0}
                                  {:llx.agent.proxy-event/type          :text-delta
                                   :llx.agent.proxy-event/content-index 0
                                   :llx.agent.proxy-event/delta         "partial"}
                                  {:llx.agent.proxy-event/type          :error
                                   :llx.agent.proxy-event/reason        :error
                                   :llx.agent.proxy-event/error-message "boom"
                                   :llx.agent.proxy-event/usage         base-usage}]
                    stream-ch    (sut/stream-proxy
                                  {}
                                  base-model
                                  {:system-prompt ""
                                   :messages      []}
                                  {:proxy-url     "https://proxy.example"
                                   :fetch-stream! (fn [_request]
                                                    (channel-from-events proxy-events))})]
                (-> (util/collect-events* stream-ch 2000)
                    (p/then (fn [events]
                              (is (= :error (:type (last events))))
                              (is (= :error (get-in (last events)
                                                    [:assistant-message :stop-reason])))
                              (is (= "boom"
                                     (get-in (last events)
                                             [:assistant-message :error-message])))))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))
