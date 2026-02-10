(ns llx.ai.impl.schema.event)

(def schemas
  {:llx/event-start
   [:map {:closed true}
    [:type [:= :start]]
    [:meta {:optional true} :map]]

   :llx/event-text-start
   [:map {:closed true}
    [:type [:= :text-start]]]

   :llx/event-text-delta
   [:map {:closed true}
    [:type [:= :text-delta]]
    [:text :string]]

   :llx/event-text-end
   [:map {:closed true}
    [:type [:= :text-end]]]

   :llx/event-thinking-start
   [:map {:closed true}
    [:type [:= :thinking-start]]]

   :llx/event-thinking-delta
   [:map {:closed true}
    [:type [:= :thinking-delta]]
    [:thinking :string]]

   :llx/event-thinking-end
   [:map {:closed true}
    [:type [:= :thinking-end]]]

   :llx/event-toolcall-start
   [:map {:closed true}
    [:type [:= :toolcall-start]]
    [:id :llx/id-string]
    [:name :llx/id-string]]

   :llx/event-toolcall-delta
   [:map {:closed true}
    [:type [:= :toolcall-delta]]
    [:id :llx/id-string]
    [:name :llx/id-string]
    [:arguments :map]]

   :llx/event-toolcall-end
   [:map {:closed true}
    [:type [:= :toolcall-end]]
    [:id :llx/id-string]
    [:name :llx/id-string]
    [:arguments :map]]

   :llx/event-done
   [:map {:closed true}
    [:type [:= :done]]
    [:assistant-message :llx/message-assistant]]

   :llx/event-error
   [:map {:closed true}
    [:type [:= :error]]
    [:assistant-message :llx/message-assistant]]

   :llx/event
   [:or
    :llx/event-start
    :llx/event-text-start
    :llx/event-text-delta
    :llx/event-text-end
    :llx/event-thinking-start
    :llx/event-thinking-delta
    :llx/event-thinking-end
    :llx/event-toolcall-start
    :llx/event-toolcall-delta
    :llx/event-toolcall-end
    :llx/event-done
    :llx/event-error]})
