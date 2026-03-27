(ns ol.llx.agent.driver
  (:require
   [ol.llx.agent.fx :as fx]
   [ol.llx.agent.loop :as loop]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(defn- public-state
  [state]
  (:public-state state))

(defn- active-run
  [state]
  (get-in state [:runtime :active-run]))

(defn- current-phase
  [state_]
  (get-in @state_ [:public-state ::loop/phase]))

(defn- cancelled?
  [signal]
  #?(:clj
     (cond
       (nil? signal) false
       (instance? java.util.concurrent.atomic.AtomicBoolean signal) (.get ^java.util.concurrent.atomic.AtomicBoolean signal)
       (instance? clojure.lang.IDeref signal) (boolean @signal)
       :else false)
     :cljs
     (cond
       (nil? signal) false
       (instance? js/AbortSignal signal) (.-aborted signal)
       (satisfies? IDeref signal) (boolean @signal)
       :else false)))

(defn- create-run-controller
  [upstream-signal]
  #?(:clj
     (let [signal      (atom false)
           cancelled?* (fn []
                         (or @signal
                             (cancelled? upstream-signal)))
           cancel!     (fn []
                         (let [already? @signal]
                           (when-not already?
                             (reset! signal true))
                           (not already?)))]
       (when (cancelled? upstream-signal)
         (cancel!))
       {:signal     signal
        :cancel!    cancel!
        :cancelled? cancelled?*})
     :cljs
     (let [controller  (when (exists? js/AbortController)
                         (js/AbortController.))
           signal      (if controller
                         (.-signal controller)
                         (atom false))
           cancelled?* (fn []
                         (or (cancelled? signal)
                             (cancelled? upstream-signal)))
           cancel!     (fn []
                         (if controller
                           (when-not (.-aborted (.-signal controller))
                             (.abort controller)
                             true)
                           (let [already? @signal]
                             (when-not already?
                               (reset! signal true))
                             (not already?))))]
       (when (cancelled? upstream-signal)
         (cancel!))
       {:signal     signal
        :cancel!    cancel!
        :cancelled? cancelled?*})))

(defn- wrap-poll-port []
  (let [ch (sp/chan)]
    (-> (p/delay 10)
        (p/then (fn [_]
                  (sp/offer ch :poll)
                  (sp/close ch))))
    ch))

(defn- step-and-collect!
  [state_ input]
  (let [[public-state' effects] (loop/step (public-state @state_) input)]
    (swap! state_ assoc :public-state public-state')
    [public-state' effects]))

(defn- resolve!
  [reply value]
  (when reply
    (p/resolve reply value)))

(defn- reject!
  [reply error]
  (when reply
    (p/reject reply error)))

(defn- settle-active-run!
  [state_]
  (when-let [run (active-run @state_)]
    (resolve! (:reply run) true)
    (swap! state_ assoc-in [:runtime :active-run] nil)
    run))

(defn- close-active-run-handles!
  [state_]
  (when-let [run (active-run @state_)]
    (doseq [[_port handle] (:effect-handles run)]
      (when-let [cancel! (:cancel! handle)]
        (cancel!)))
    (swap! state_ assoc-in [:runtime :active-run :effect-handles] {})))

(defn- remove-handle!
  [state_ port]
  (when (active-run @state_)
    (swap! state_ update-in [:runtime :active-run :effect-handles] dissoc port)))

(defn- process-effects!
  [env effects]
  (reduce
   (fn [ports effect]
     (let [result (fx/execute-fx env effect)]
       (if (and (map? result) (:signals> result))
         (let [port (:signals> result)]
           (swap! (:state_ env) assoc-in [:runtime :active-run :effect-handles port] result)
           (conj ports port))
         ports)))
   []
   effects))

(defn- create-active-run!
  [state_ upstream-signal reply]
  (let [{:keys [signal cancel! cancelled?]} (create-run-controller upstream-signal)
        run-id                              (get-in @state_ [:runtime :next-run-id])
        run-record                          {:id             run-id
                                             :signal         signal
                                             :cancel!        cancel!
                                             :cancelled?     cancelled?
                                             :effect-handles {}
                                             :reply          reply}]
    (swap! state_ (fn [state]
                    (-> state
                        (assoc-in [:runtime :active-run] run-record)
                        (update-in [:runtime :next-run-id] inc))))
    run-record))

(defn- stale-signal?
  [state_ signal]
  (when-let [run-id (:run-id signal)]
    (not= run-id (get-in @state_ [:runtime :active-run :id]))))

(defn- maybe-settle-run-after-step!
  [state_]
  (when (and (= ::loop/idle (current-phase state_))
             (active-run @state_))
    (settle-active-run! state_)))

(defn- process-signal!
  [env active signal]
  (let [state_ (:state_ env)]
    (if (stale-signal? state_ signal)
      active
      (let [[_public-state effects] (step-and-collect! state_ signal)
            new-ports               (process-effects! env effects)]
        (maybe-settle-run-after-step! state_)
        (into active new-ports)))))

(defn- process-effect-signal!
  [env active port signal]
  (let [signal (assoc signal :source-port port)]
    (process-signal! env active signal)))

(defn- run-abort!
  [env reply]
  (let [state_ (:state_ env)]
    (close-active-run-handles! state_)
    (when-let [cancel! (get-in @state_ [:runtime :active-run :cancel!])]
      (cancel!))
    (let [[_public-state effects] (step-and-collect! state_ {:type :ol.llx.agent.signal/abort})
          new-ports               (process-effects! env effects)]
      (settle-active-run! state_)
      (resolve! reply true)
      (into #{} new-ports))))

(defn- close-runtime!
  [env reply]
  (let [state_ (:state_ env)
        phase  (current-phase state_)]
    (when (active-run @state_)
      (close-active-run-handles! state_)
      (when-let [cancel! (get-in @state_ [:runtime :active-run :cancel!])]
        (cancel!))
      (let [[_public-state effects] (step-and-collect! state_ {:type :ol.llx.agent.signal/abort})]
        (process-effects! env effects)
        (settle-active-run! state_)))
    (swap! state_ (fn [state]
                    (-> state
                        (assoc-in [:runtime :closing?] true)
                        (assoc-in [:public-state ::loop/phase] ::loop/closed))))
    (resolve! reply (or (= phase ::loop/closed) true))
    (sp/close (:events-mx> env))
    (sp/close (:command> env))
    :stop))

(defn- process-command!
  [env active {:keys [command reply]}]
  (let [state_ (:state_ env)]
    (case (:type command)
      :ol.llx.agent.command/close
      (close-runtime! env reply)

      :ol.llx.agent.command/abort
      (if (active-run @state_)
        (run-abort! env reply)
        (let [[_public-state effects] (step-and-collect! state_ command)]
          (process-effects! env effects)
          (resolve! reply false)
          active))

      (let [phase-before            (current-phase state_)
            [public-state' effects] (step-and-collect! state_ command)
            start-run?              (and (= phase-before ::loop/idle)
                                         (nil? (active-run @state_))
                                         (#{:ol.llx.agent.command/prompt :ol.llx.agent.command/continue} (:type command))
                                         (not= (::loop/phase public-state') ::loop/idle))
            _                       (when start-run?
                                      (create-active-run! state_ (:abort-signal env) reply))
            new-ports               (process-effects! env effects)]
        (when (and reply (not start-run?))
          (if (#{:ol.llx.agent.command/prompt
                 :ol.llx.agent.command/continue}
               (:type command))
            (resolve! reply false)
            (resolve! reply true)))
        (maybe-settle-run-after-step! state_)
        (into active new-ports)))))

(defn- driver-loop
  [env]
  (let [state_   (:state_ env)
        command> (:command> env)]
    (p/loop [active #{}]
      (let [poll-port (when (active-run @state_)
                        (wrap-poll-port))
            ports     (cond-> [command>]
                        (seq active) (into active)
                        poll-port (conj poll-port))]
        (p/let [[val port] (sp/alts ports)]
          (cond
            (nil? port)
            nil

            (= port command>)
            (if (nil? val)
              nil
              (let [result (process-command! env active val)]
                (if (= :stop result)
                  nil
                  (p/recur (set result)))))

            (= port poll-port)
            (if (and (active-run @state_)
                     ((get-in @state_ [:runtime :active-run :cancelled?])))
              (p/recur (run-abort! env nil))
              (p/recur active))

            (nil? val)
            (do
              (remove-handle! state_ port)
              (p/recur (disj active port)))

            :else
            (p/recur (process-effect-signal! env active port val))))))))

(defn start!
  [env]
  (-> (driver-loop env)
      (p/catch (fn [error]
                 (when-let [run (active-run @(:state_ env))]
                   (reject! (:reply run) error))
                 (sp/close (:events-mx> env))
                 (sp/close (:command> env))
                 (throw error)))))

(defn run
  [env command]
  (let [reply (p/deferred)]
    (-> (sp/put (:command> env) {:command command :reply reply})
        (p/then (fn [accepted?]
                  (if accepted?
                    reply
                    false))))))
