(ns llx.agent.runtime-queue-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]]
       :cljs [[cljs.test :refer-macros [deftest is]]])
   #?@(:clj [[llx.ai.test-util :as util]]
       :cljs [[llx.ai.test-util :as util :include-macros true]])
   [llx.agent.runtime :as sut]
   [promesa.core :as p]))

;; Queue behavior tests for the runtime command surface.

(def assistant-tail
  [{:role      :user
    :content   [{:type :text :text "hello"}]
    :timestamp 1730000000000}
   {:role        :assistant
    :content     [{:type :text :text "done"}]
    :api         :openai-completions
    :provider    :openai
    :model       "gpt-4o-mini"
    :usage       {:input        0
                  :output       0
                  :cache-read   0
                  :cache-write  0
                  :total-tokens 0
                  :cost         {:input 0.0 :output 0.0 :cache-read 0.0 :cache-write 0.0 :total 0.0}}
    :stop-reason :stop
    :timestamp   1730000000001}])

(defn- mk-message
  [text ts]
  {:role :user :content [{:type :text :text text}] :timestamp ts})

(defn- runtime-with-calls
  [opts]
  (let [calls* (atom [])
        run!   (fn [input]
                 (swap! calls* conj input)
                 {:result  (p/resolved {:status :ok})
                  :cancel! (fn [] nil)})]
    {:runtime (sut/create-runtime (merge {:initial-state {:messages assistant-tail}
                                          :run-command!  run!}
                                         opts))
     :calls*  calls*}))

(deftest continue-uses-one-at-a-time-steering-mode
  (util/async done
              (let [{:keys [runtime calls*]} (runtime-with-calls {:steering-mode :one-at-a-time})
                    s1                       (mk-message "s1" 1)
                    s2                       (mk-message "s2" 2)]
                (-> (sut/steer! runtime s1)
                    (p/then (fn [_]
                              (sut/steer! runtime s2)))
                    (p/then (fn [_]
                              (sut/continue! runtime)))
                    (p/then (fn [_]
                              (is (= [s1] (:messages (first @calls*))))
                              (is (= [s2] (:steering-queue (sut/state runtime))))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest continue-uses-all-steering-mode
  (util/async done
              (let [{:keys [runtime calls*]} (runtime-with-calls {:steering-mode :all})
                    s1                       (mk-message "s1" 1)
                    s2                       (mk-message "s2" 2)]
                (-> (sut/steer! runtime s1)
                    (p/then (fn [_]
                              (sut/steer! runtime s2)))
                    (p/then (fn [_]
                              (sut/continue! runtime)))
                    (p/then (fn [_]
                              (is (= [s1 s2] (:messages (first @calls*))))
                              (is (= [] (:steering-queue (sut/state runtime))))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest continue-uses-one-at-a-time-follow-up-mode
  (util/async done
              (let [{:keys [runtime calls*]} (runtime-with-calls {:follow-up-mode :one-at-a-time})
                    f1                       (mk-message "f1" 1)
                    f2                       (mk-message "f2" 2)]
                (-> (sut/follow-up! runtime f1)
                    (p/then (fn [_]
                              (sut/follow-up! runtime f2)))
                    (p/then (fn [_]
                              (sut/continue! runtime)))
                    (p/then (fn [_]
                              (is (= [f1] (:messages (first @calls*))))
                              (is (= [f2] (:follow-up-queue (sut/state runtime))))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest continue-uses-all-follow-up-mode
  (util/async done
              (let [{:keys [runtime calls*]} (runtime-with-calls {:follow-up-mode :all})
                    f1                       (mk-message "f1" 1)
                    f2                       (mk-message "f2" 2)]
                (-> (sut/follow-up! runtime f1)
                    (p/then (fn [_]
                              (sut/follow-up! runtime f2)))
                    (p/then (fn [_]
                              (sut/continue! runtime)))
                    (p/then (fn [_]
                              (is (= [f1 f2] (:messages (first @calls*))))
                              (is (= [] (:follow-up-queue (sut/state runtime))))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest queue-clear-and-presence-helpers
  (util/async done
              (let [{:keys [runtime]} (runtime-with-calls {})
                    s1                (mk-message "s1" 1)
                    f1                (mk-message "f1" 2)]
                (-> (sut/steer! runtime s1)
                    (p/then (fn [_]
                              (sut/follow-up! runtime f1)))
                    (p/then (fn [_]
                              (is (true? (sut/has-queued-messages? runtime)))
                              (sut/clear-steering-queue! runtime)))
                    (p/then (fn [_]
                              (is (= [] (:steering-queue (sut/state runtime))))
                              (is (= [f1] (:follow-up-queue (sut/state runtime))))
                              (sut/clear-follow-up-queue! runtime)))
                    (p/then (fn [_]
                              (is (= [] (:follow-up-queue (sut/state runtime))))
                              (is (false? (sut/has-queued-messages? runtime)))
                              (sut/steer! runtime s1)))
                    (p/then (fn [_]
                              (sut/follow-up! runtime f1)))
                    (p/then (fn [_]
                              (sut/clear-all-queues! runtime)))
                    (p/then (fn [_]
                              (is (= [] (:steering-queue (sut/state runtime))))
                              (is (= [] (:follow-up-queue (sut/state runtime))))
                              (is (false? (sut/has-queued-messages? runtime)))
                              (sut/close! runtime)))
                    (p/then (fn [_]
                              (done)))
                    (p/catch (partial util/fail-and-done! done))))))

(deftest runtime-handle-contains-single-command-channel
  (let [{:keys [runtime]} (runtime-with-calls {})]
    (is (map? runtime))
    (is (contains? runtime :runtime-state*))
    (is (contains? runtime :submit-command!))
    (is (contains? runtime :fsm-env))))

#?(:clj
   (deftest runtime-cljc-uses-non-blocking-csp-apis
     (let [source (slurp "src/llx/agent/runtime.cljc")]
       (is (not (re-find #"<!!|>!!|alts!!|go-loop|thread" source)))))
   :cljs
   (deftest runtime-cljc-uses-non-blocking-csp-apis
     (is true)))

#?(:clj
   (deftest runtime-command-guardrails-avoid-any-signatures
     (let [source (slurp "src/llx/agent/runtime.cljc")]
       (is (not (re-find #":any\\s+=>\\s+:any" source)))))
   :cljs
   (deftest runtime-command-guardrails-avoid-any-signatures
     (is true)))
