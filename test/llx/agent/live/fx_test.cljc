(ns ^{:kaocha/parallelize? true} llx.agent.live.fx-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]
             [llx.ai.test-util :as util]]
       :cljs [[cljs.test :refer [deftest is]]
              [llx.ai.test-util :as util :include-macros true]])
   [llx.ai :as ai]
   [llx.agent.fx :as fx]
   [llx.ai.live.models :as models]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

#?(:clj (set! *warn-on-reflection* true))

(defn- empty-queue
  []
  #?(:clj clojure.lang.PersistentQueue/EMPTY
     :cljs #queue []))

(defn- agent-state
  [model]
  {:llx.agent.loop/phase :llx.agent.loop/streaming
   :system-prompt        "You are concise."
   :model                model
   :thinking-level       :off
   :tools                []
   :messages             []
   :stream-message       nil
   :pending-tool-calls   []
   :error                nil
   :steering-queue       (empty-queue)
   :follow-up-queue      (empty-queue)
   :steering-mode        :one-at-a-time
   :follow-up-mode       :one-at-a-time})

(defn- collect-signals*
  [ch timeout-ms]
  (p/loop [acc []]
    (p/let [signal (sp/take ch timeout-ms ::timeout)]
      (prn signal)
      (cond
        (= ::timeout signal)
        (p/rejected (ex-info "Timed out waiting for fx-call-llm signals"
                             {:timeout-ms   timeout-ms
                              :signal-count (count acc)}))

        (nil? signal)
        acc

        :else
        (p/recur (conj acc signal))))))

(deftest ^:llx/openai live-fx-call-llm-openai
  (util/async done
              (util/run-live-async!
               (let [env    {:state_         (atom (agent-state models/openai-completions))
                             :convert-to-llm identity
                             :stream-fn      (fn [model context options]
                                               (ai/stream (ai/default-env)
                                                          model
                                                          context
                                                          (assoc options :max-tokens 64)))}
                     effect {::fx/type :call-llm
                             :messages [{:role      :user
                                         :content   "Reply with exactly: llx agent fx live ok"
                                         :timestamp 1}]}
                     out    (fx/execute-fx env effect)]
                 (-> (collect-signals* out 90000)
                     (p/then (fn [signals]
                               (is (seq signals))
                               (let [terminal (last signals)]
                                 (when (= :signal/llm-error (:type terminal))
                                   (throw (ex-info "Live provider call returned :signal/llm-error"
                                                   {:signal-count (count signals)
                                                    :error        (:error terminal)}))))
                               (is (= :signal/llm-start (:type (first signals))))
                               (is (= :signal/llm-done (:type (last signals))))
                               (let [assistant-message (:message (last signals))]
                                 (is (= :assistant (:role assistant-message)))
                                 (is (#{:stop :length :tool-use} (:stop-reason assistant-message)))
                                 (is (seq (:content assistant-message))))
                               true))))
               done)))
