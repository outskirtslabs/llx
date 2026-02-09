(ns llx-ai.transform-messages
  (:require
   [clojure.string :as str]))

(defn- same-model?
  [assistant-message target-model]
  (and (= (:provider assistant-message) (:provider target-model))
       (= (:api assistant-message) (:api target-model))
       (= (:model assistant-message) (:id target-model))))

(defn- synth-tool-result
  [tool-call now-ms]
  {:role         :tool-result
   :tool-call-id (:id tool-call)
   :tool-name    (:name tool-call)
   :content      [{:type :text :text "No result provided"}]
   :is-error?    true
   :timestamp    (now-ms)})

(defn- valid-tool-call?
  [content-block]
  (and (= :tool-call (:type content-block))
       (string? (:id content-block))
       (not (str/blank? (:id content-block)))
       (string? (:name content-block))
       (not (str/blank? (:name content-block)))))

(defn- transform-assistant-content
  [assistant-message target-model normalize-tool-call-id tool-call-id-map*]
  (let [same-model? (same-model? assistant-message target-model)]
    (->> (:content assistant-message)
         (mapcat
          (fn [block]
            (case (:type block)
              :thinking
              (let [thinking       (:thinking block)
                    has-signature? (string? (:signature block))]
                (cond
                  (and same-model? has-signature?) [block]
                  (str/blank? (or thinking "")) []
                  same-model? [block]
                  :else [{:type :text :text thinking}]))

              :tool-call
              (let [normalized-block (if (and (not same-model?) (contains? block :signature))
                                       (dissoc block :signature)
                                       block)
                    source-id        (:id block)]
                (if (and (not same-model?)
                         normalize-tool-call-id
                         (string? source-id)
                         (not (str/blank? source-id)))
                  (let [normalized-id (normalize-tool-call-id source-id target-model assistant-message)]
                    (if (and (string? normalized-id)
                             (not= source-id normalized-id))
                      (do
                        (swap! tool-call-id-map* assoc source-id normalized-id)
                        [(assoc normalized-block :id normalized-id)])
                      [normalized-block]))
                  [normalized-block]))

              [block])))
         vec)))

(defn- transform-message-first-pass
  [message target-model normalize-tool-call-id tool-call-id-map*]
  (case (:role message)
    :assistant
    (assoc message
           :content (transform-assistant-content
                     message
                     target-model
                     normalize-tool-call-id
                     tool-call-id-map*))

    :tool-result
    (if-let [normalized-id (get @tool-call-id-map* (:tool-call-id message))]
      (assoc message :tool-call-id normalized-id)
      message)

    message))

(defn- flush-pending-tool-calls
  [result pending-tool-calls existing-tool-result-ids now-ms]
  (reduce
   (fn [acc tool-call]
     (if (contains? existing-tool-result-ids (:id tool-call))
       acc
       (conj acc (synth-tool-result tool-call now-ms))))
   result
   pending-tool-calls))

(defn- second-pass
  [messages now-ms]
  (loop [remaining                messages
         result                   []
         pending-tool-calls       []
         existing-tool-result-ids #{}]
    (if-let [message (first remaining)]
      (case (:role message)
        :assistant
        (let [result                   (if (seq pending-tool-calls)
                                         (flush-pending-tool-calls
                                          result
                                          pending-tool-calls
                                          existing-tool-result-ids
                                          now-ms)
                                         result)
              pending-tool-calls       []
              existing-tool-result-ids #{}]
          (if (#{:error :aborted} (:stop-reason message))
            (recur (rest remaining) result pending-tool-calls existing-tool-result-ids)
            (let [tool-calls (filterv valid-tool-call? (:content message))]
              (recur (rest remaining)
                     (conj result message)
                     tool-calls
                     #{}))))

        :tool-result
        (recur (rest remaining)
               (conj result message)
               pending-tool-calls
               (cond-> existing-tool-result-ids
                 (string? (:tool-call-id message)) (conj (:tool-call-id message))))

        :user
        (let [result (if (seq pending-tool-calls)
                       (flush-pending-tool-calls
                        result
                        pending-tool-calls
                        existing-tool-result-ids
                        now-ms)
                       result)]
          (recur (rest remaining)
                 (conj result message)
                 []
                 #{}))

        (recur (rest remaining)
               (conj result message)
               pending-tool-calls
               existing-tool-result-ids))
      result)))

(defn for-target-model
  [messages target-model opts]
  (let [now-ms                 (or (:clock/now-ms opts)
                                   #?(:clj (fn [] (System/currentTimeMillis))
                                      :cljs (fn [] (.now js/Date))))
        normalize-tool-call-id (:normalize-tool-call-id opts)
        tool-call-id-map*      (atom {})
        first-pass             (mapv (fn [message]
                                       (transform-message-first-pass
                                        message
                                        target-model
                                        normalize-tool-call-id
                                        tool-call-id-map*))
                                     messages)]
    (second-pass first-pass now-ms)))
