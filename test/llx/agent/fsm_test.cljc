(ns llx.agent.fsm-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is]]]
       :cljs [[cljs.test :refer-macros [deftest is]]])
   [llx.agent.fsm :as fsm]))

(deftest starts-in-idle
  (let [env (fsm/new-env)]
    (fsm/start! env)
    (is (fsm/in-state? env ::fsm/runtime))
    (is (fsm/in-state? env ::fsm/idle))
    (is (not (fsm/in-state? env ::fsm/starting)))
    (is (not (fsm/in-state? env ::fsm/running)))
    (is (not (fsm/in-state? env ::fsm/closed)))))

(deftest progresses-through-turn-lifecycle
  (let [env (fsm/new-env)]
    (fsm/start! env)

    (fsm/send! env fsm/cmd-prompt)
    (is (fsm/in-state? env ::fsm/starting))

    (fsm/send! env fsm/runner-started)
    (is (fsm/in-state? env ::fsm/running))

    (fsm/send! env fsm/cmd-abort)
    (is (fsm/in-state? env ::fsm/running))

    (fsm/send! env fsm/runner-succeeded)
    (is (fsm/in-state? env ::fsm/idle))))

(deftest reset-and-shutdown-transitions
  (let [env (fsm/new-env)]
    (fsm/start! env)

    (fsm/send! env fsm/cmd-prompt)
    (is (fsm/in-state? env ::fsm/starting))
    (fsm/send! env fsm/cmd-reset)
    (is (fsm/in-state? env ::fsm/idle))

    (fsm/send! env fsm/cmd-prompt)
    (fsm/send! env fsm/runner-started)
    (is (fsm/in-state? env ::fsm/running))
    (fsm/send! env fsm/cmd-shutdown)
    (is (fsm/in-state? env ::fsm/closed))

    ;; Closed is absorbing in this initial model.
    (fsm/send! env fsm/runner-failed)
    (fsm/send! env fsm/cmd-reset)
    (is (fsm/in-state? env ::fsm/closed))
    (is (not (fsm/in-state? env ::fsm/idle)))))

(deftest transition-plan-includes-effects
  (let [env (fsm/new-env)]
    (fsm/start! env)

    (let [step (fsm/send! env (fsm/event fsm/cmd-prompt {:messages [:hello]}))]
      (is (= ::fsm/idle (:before step)))
      (is (= ::fsm/starting (:after step)))
      (is (= ::fsm/starting (:planned-next-state step)))
      (is (= [::fsm/fx-start-turn ::fsm/fx-invoke-runner]
             (mapv :op (:effects step))))
      (is (= :prompt (get-in step [:effects 1 :command]))))

    (fsm/send! env fsm/runner-started)
    (let [step (fsm/send! env fsm/cmd-abort)]
      (is (= ::fsm/running (:before step)))
      (is (= ::fsm/running (:after step)))
      (is (= [::fsm/fx-cancel-runner ::fsm/fx-resolve-command]
             (mapv :op (:effects step)))))

    (let [step (fsm/send! env (fsm/event fsm/runner-failed {:error "boom"}))]
      (is (= ::fsm/running (:before step)))
      (is (= ::fsm/idle (:after step)))
      (is (= [::fsm/fx-finish-turn ::fsm/fx-resolve-waiters ::fsm/fx-reject-active]
             (mapv :op (:effects step))))
      (is (= {:error "boom"}
             (get-in step [:effects 0 :error]))))))

(deftest closed-state-rejects-commands-via-effects
  (let [env (fsm/new-env)]
    (fsm/start! env)
    (fsm/send! env fsm/cmd-shutdown)
    (is (fsm/in-state? env ::fsm/closed))

    (let [step (fsm/send! env fsm/cmd-prompt)]
      (is (= ::fsm/closed (:before step)))
      (is (= ::fsm/closed (:after step)))
      (is (= [::fsm/fx-reject-command]
             (mapv :op (:effects step))))
      (is (= :runtime-closed
             (get-in step [:effects 0 :reason]))))))
