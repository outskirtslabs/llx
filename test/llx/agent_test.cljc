(ns llx.agent-test
  (:require
   #?@(:clj [[clojure.test :refer [deftest is testing]]]
       :cljs [[cljs.test :refer-macros [deftest is testing]]])
   [com.fulcrologic.guardrails.malli.registry :as gr.reg]
   [llx.ai :as ai]
   [llx.agent :as sut]
   [llx.agent.driver :as driver]
   [llx.agent.schema :as agent.schema]
   [malli.registry :as mr]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(defn- closed-channel
  []
  (let [ch (sp/chan)]
    (sp/close ch)
    ch))

(def required-env-opts
  {:convert-to-llm    identity
   :transform-context identity
   :stream-fn         (fn [_model _context _opts]
                        (closed-channel))
   :tool-defs         {}})

(defn- merge-custom-message-guardrails-schemas!
  [custom-message-schemas schema-registry]
  (gr.reg/merge-schemas!
   (merge (agent.schema/schemas {:custom-message-schemas custom-message-schemas})
          schema-registry)))

(defn- base-state
  [messages]
  {:llx.agent.loop/phase :llx.agent.loop/idle
   :system-prompt        ""
   :model                (ai/get-model :openai "gpt-5.2-codex")
   :thinking-level       :off
   :tools                []
   :messages             messages
   :stream-message       nil
   :pending-tool-calls   []
   :error                nil
   :steering-queue       #?(:clj clojure.lang.PersistentQueue/EMPTY
                            :cljs #queue [])
   :follow-up-queue      #?(:clj clojure.lang.PersistentQueue/EMPTY
                            :cljs #queue [])
   :steering-mode        :one-at-a-time
   :follow-up-mode       :one-at-a-time})

(deftest create-agent-initializes-state-test
  (let [model (ai/get-model :openai "gpt-4o")
        agent (sut/create-agent (merge required-env-opts
                                       {:system-prompt  "System prompt"
                                        :model          model
                                        :thinking-level :high
                                        :tools          [{:name "read_file"}]
                                        :steering-mode  :all
                                        :follow-up-mode :all}))
        state (sut/state agent)]
    (is (= "System prompt" (:system-prompt state)))
    (is (= model (:model state)))
    (is (= :high (:thinking-level state)))
    (is (= [{:name "read_file"}] (:tools state)))
    (is (= [] (:messages state)))
    (is (= :all (:steering-mode state)))
    (is (= :all (:follow-up-mode state)))))

(deftest create-agent-requires-env-contract-test
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                  :cljs js/Error)
               (sut/create-agent {}))))

(deftest create-agent-ignores-initial-state-option-test
  (let [agent (sut/create-agent (assoc required-env-opts
                                       :initial-state {:messages [{:role :user :content "old"}]}))]
    (is (= [] (:messages (sut/state agent))))))

(deftest create-agent-default-convert-to-llm-test
  (gr.reg/merge-schemas!
   (merge (agent.schema/schemas {:custom-message-schemas {:custom       :test/message-custom
                                                          :notification :test/message-notification}})
          {:test/message-custom
           [:map
            [:role [:= :custom]]
            [:content :string]]
           :test/message-notification
           [:map
            [:role [:= :notification]]
            [:content :string]]}))
  (let [agent          (sut/create-agent (dissoc required-env-opts :convert-to-llm))
        convert-to-llm (:convert-to-llm agent)
        input          [{:role :user :content "u" :timestamp 1}
                        {:role :custom :content "skip"}
                        {:role        :assistant
                         :content     [{:type :text :text "assistant"}]
                         :api         :openai-responses
                         :provider    :openai
                         :model       "gpt-4o"
                         :usage       {:input        0
                                       :output       0
                                       :cache-read   0
                                       :cache-write  0
                                       :total-tokens 0
                                       :cost         {:input       0
                                                      :output      0
                                                      :cache-read  0
                                                      :cache-write 0
                                                      :total       0}}
                         :stop-reason :stop
                         :timestamp   2}
                        {:role         :tool-result
                         :tool-call-id "tc1"
                         :tool-name    "tool"
                         :content      [{:type :text :text "result"}]
                         :is-error?    false
                         :timestamp    3}
                        {:role :notification :content "skip"}]]
    (is (= [{:role :user :content "u" :timestamp 1}
            {:role        :assistant
             :content     [{:type :text :text "assistant"}]
             :api         :openai-responses
             :provider    :openai
             :model       "gpt-4o"
             :usage       {:input        0
                           :output       0
                           :cache-read   0
                           :cache-write  0
                           :total-tokens 0
                           :cost         {:input       0
                                          :output      0
                                          :cache-read  0
                                          :cache-write 0
                                          :total       0}}
             :stop-reason :stop
             :timestamp   2}
            {:role         :tool-result
             :tool-call-id "tc1"
             :tool-name    "tool"
             :content      [{:type :text :text "result"}]
             :is-error?    false
             :timestamp    3}]
           (convert-to-llm input)))))

(deftest rehydrate-agent-uses-snapshot-test
  (let [model (ai/get-model :openai "gpt-4o")
        state {:llx.agent.loop/phase :llx.agent.loop/idle
               :system-prompt        "Hydrated"
               :model                model
               :thinking-level       :medium
               :tools                [{:name "read_file"}]
               :messages             [{:role :user :content "old" :timestamp 1}]
               :stream-message       nil
               :pending-tool-calls   []
               :error                nil
               :steering-queue       #?(:clj clojure.lang.PersistentQueue/EMPTY
                                        :cljs #queue [])
               :follow-up-queue      #?(:clj clojure.lang.PersistentQueue/EMPTY
                                        :cljs #queue [])
               :steering-mode        :one-at-a-time
               :follow-up-mode       :one-at-a-time}
        agent (sut/rehydrate-agent state required-env-opts)]
    (is (= state (sut/state agent)))))

(deftest subscribe-unsubscribe-test
  (let [agent      (sut/create-agent required-env-opts)
        default-ch (sut/subscribe agent)
        custom-ch  (sp/chan :buf (sp/sliding-buffer 8))
        event-1    {:type :llx.agent.event/test :n 1}
        event-2    {:type :llx.agent.event/test :n 2}]
    (is (sp/chan? default-ch))
    (is (identical? custom-ch (sut/subscribe agent custom-ch)))

    (sp/offer (:events-mx> agent) event-1)
    #?(:clj
       (do
         (is (= event-1 (sp/take! default-ch 200 ::timeout)))
         (is (= event-1 (sp/take! custom-ch 200 ::timeout))))
       :cljs
       (is true))

    (sut/unsubscribe agent default-ch)
    (sut/unsubscribe agent custom-ch)
    (is (sp/closed? default-ch))
    (is (sp/closed? custom-ch))

    (sp/offer (:events-mx> agent) event-2)
    #?(:clj
       (do
         (is (nil? (sp/take! default-ch 50 ::timeout)))
         (is (nil? (sp/take! custom-ch 50 ::timeout))))
       :cljs
       (is true))))

(deftest command-wrappers-dispatch-through-driver-run-test
  (let [seen*  (atom [])
        marker (p/resolved ::marker)
        model  (ai/get-model :openai "gpt-4o")
        agent  (sut/create-agent required-env-opts)]
    (with-redefs [driver/run (fn [env input]
                               (swap! seen* conj [env input])
                               marker)]
      (testing "prompt dispatches vector messages"
        (is (identical? marker (sut/prompt agent [{:role :user :content "hello" :timestamp 1}]))))

      (testing "continue dispatches the continue command"
        (is (identical? marker (sut/continue agent))))

      (testing "abort dispatches the abort command"
        (is (identical? marker (sut/abort agent))))

      (testing "state mutators dispatch commands"
        (is (identical? marker (sut/set-model agent model)))
        (is (identical? marker (sut/set-system-prompt agent "new prompt"))))

      (is (= [{:type     :llx.agent.command/prompt
               :messages [{:role :user :content "hello" :timestamp 1}]}
              {:type :llx.agent.command/continue}
              {:type :llx.agent.command/abort}
              {:type :llx.agent.command/set-model :model model}
              {:type :llx.agent.command/set-system-prompt :system-prompt "new prompt"}]
             (mapv second @seen*))))))

(deftest prompt-validates-messages-test
  (let [seen*  (atom [])
        marker (p/resolved ::marker)
        agent  (sut/create-agent required-env-opts)]
    (with-redefs [driver/run (fn [_env input]
                               (swap! seen* conj input)
                               marker)]
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                      :cljs js/Error)
                   (sut/prompt agent {:role :user :content "not-a-vector"})))
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                      :cljs js/Error)
                   (sut/prompt agent [nil {:role :user :content "ok" :timestamp 1}])))
      (is (identical? marker (sut/prompt agent [{:role :user :content "ok" :timestamp 2}])))
      (is (= [{:type     :llx.agent.command/prompt
               :messages [{:role :user :content "ok" :timestamp 2}]}]
             @seen*)))))

(deftest set-model-validates-model-shape-test
  (let [seen*       (atom [])
        marker      (p/resolved ::marker)
        valid-model (ai/get-model :openai "gpt-4o")
        agent       (sut/create-agent required-env-opts)]
    (with-redefs [driver/run (fn [_env input]
                               (swap! seen* conj input)
                               marker)]
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                      :cljs js/Error)
                   (sut/set-model agent {:id "model-2"})))
      (is (identical? marker (sut/set-model agent valid-model)))
      (is (= [{:type :llx.agent.command/set-model :model valid-model}]
             @seen*)))))

(deftest custom-message-schemas-validate-message-and-messages-test
  (let [custom-message          {:role      :my-app/notification
                                 :text      "Heads up"
                                 :timestamp 1}
        canonical-user-message  {:role      :user
                                 :content   "hello"
                                 :timestamp 2}
        custom-message-invalid  (dissoc custom-message :text)
        base-registry           (agent.schema/registry {})
        custom-enabled-registry (mr/composite-registry
                                 (agent.schema/registry {:custom-message-schemas {:my-app/notification :my/message-notification}})
                                 {:my/message-notification
                                  [:map
                                   [:role [:= :my-app/notification]]
                                   [:text :string]
                                   [:timestamp :int]]})]
    (testing ":llx.agent/custom-message-schemas requires namespace-qualified dispatch keys"
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                      :cljs js/Error)
                   (agent.schema/validate! base-registry
                                           :llx.agent/custom-message-schemas
                                           {:notification :my/message-notification}))))

    (testing ":llx.agent/message rejects custom roles when not registered"
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                      :cljs js/Error)
                   (agent.schema/validate! base-registry :llx.agent/message custom-message))))

    (testing ":llx.agent/message accepts registered custom role"
      (is (= custom-message
             (agent.schema/validate! custom-enabled-registry :llx.agent/message custom-message))))

    (testing ":llx.agent/messages accepts canonical + registered custom messages"
      (is (= [canonical-user-message custom-message]
             (agent.schema/validate! custom-enabled-registry
                                     :llx.agent/messages
                                     [canonical-user-message custom-message]))))

    (testing ":llx.agent/messages rejects invalid registered custom message payloads"
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo
                      :cljs js/Error)
                   (agent.schema/validate! custom-enabled-registry
                                           :llx.agent/messages
                                           [canonical-user-message custom-message-invalid]))))

    (testing "canonical messages remain valid with custom registrations enabled"
      (is (= canonical-user-message
             (agent.schema/validate! custom-enabled-registry
                                     :llx.agent/message
                                     canonical-user-message))))))

(deftest custom-message-public-api-validation-test
  (let [custom-message-schemas {:my-app/notification :my/message-notification}
        custom-schema-registry {:my/message-notification
                                [:map
                                 [:role [:= :my-app/notification]]
                                 [:text :string]
                                 [:timestamp :int]]}
        _                      (merge-custom-message-guardrails-schemas!
                                custom-message-schemas
                                custom-schema-registry)
        custom-message         {:role :my-app/notification :text "Heads up" :timestamp 1}
        agent                  (sut/create-agent
                                (merge required-env-opts
                                       {:custom-message-schemas custom-message-schemas
                                        :schema-registry        custom-schema-registry}))
        seen*                  (atom [])
        marker                 (p/resolved ::ok)]
    (with-redefs [driver/run (fn [_env command]
                               (swap! seen* conj command)
                               marker)]
      (is (identical? marker (sut/prompt agent [custom-message])))
      (is (identical? marker (sut/append-message agent custom-message)))
      (is (identical? marker (sut/replace-messages agent [custom-message]))))
    (is (= [{:type :llx.agent.command/prompt :messages [custom-message]}
            {:type :llx.agent.command/append-message :message custom-message}
            {:type :llx.agent.command/replace-messages :messages [custom-message]}]
           @seen*))))

(deftest rehydrate-agent-custom-message-validation-test
  (let [custom-message {:role :my-app/notification :text "rehydrated" :timestamp 1}
        opts           (merge required-env-opts
                              {:custom-message-schemas {:my-app/notification :my/message-notification}
                               :schema-registry        {:my/message-notification
                                                        [:map
                                                         [:role [:= :my-app/notification]]
                                                         [:text :string]
                                                         [:timestamp :int]]}})
        valid-state    (base-state [custom-message])]
    (is (= valid-state
           (sut/state (sut/rehydrate-agent valid-state opts))))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (sut/rehydrate-agent (update-in valid-state [:messages 0] dissoc :text) opts)))))

(deftest create-agent-rejects-missing-custom-schema-registration-test
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (sut/create-agent
                (merge required-env-opts
                       {:custom-message-schemas {:my-app/notification :my/missing-schema}
                        :schema-registry        {:my/other-schema [:map [:x :int]]}})))))

(deftest custom-message-schema-may-reference-provided-schemas-test
  (let [custom-message-schemas {:my-app/rich-note :my/rich-note-message}
        custom-schema-registry {:my/rich-note-payload [:map [:text :string]]
                                :my/rich-note-message [:map
                                                       [:role [:= :my-app/rich-note]]
                                                       [:payload :my/rich-note-payload]
                                                       [:timestamp :int]]}
        _                      (merge-custom-message-guardrails-schemas!
                                custom-message-schemas
                                custom-schema-registry)
        valid-message          {:role      :my-app/rich-note
                                :payload   {:text "hello"}
                                :timestamp 1}
        invalid-message        (assoc valid-message :payload {})
        agent                  (sut/create-agent
                                (merge required-env-opts
                                       {:custom-message-schemas custom-message-schemas
                                        :schema-registry        custom-schema-registry}))
        seen*                  (atom [])
        marker                 (p/resolved ::ok)]
    (with-redefs [driver/run (fn [_env command]
                               (swap! seen* conj command)
                               marker)]
      (is (identical? marker (sut/prompt agent [valid-message])))
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (sut/prompt agent [invalid-message]))))
    (is (= [{:type :llx.agent.command/prompt :messages [valid-message]}]
           @seen*))))
