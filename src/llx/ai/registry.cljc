(ns llx.ai.registry
  (:require
   [llx.ai.impl.registry :as impl.registry]))

(defn immutable-registry
  "Creates an immutable registry map.

  With no argument it creates an empty normalized registry."
  ([]
   (impl.registry/immutable-registry))
  ([registry-map]
   (impl.registry/immutable-registry registry-map)))

(defn mutable-registry
  "Wraps an atom-backed registry for mutable use cases."
  [registry*]
  (impl.registry/mutable-registry registry*))

(defn dynamic-registry
  "Creates a dynamic registry wrapper that resolves from thread-local bindings."
  []
  (impl.registry/dynamic-registry))

(defn default-registry
  "Returns the process default registry."
  []
  (impl.registry/default-registry))

(defn set-default-registry!
  "Sets the process default registry after resolving wrapper implementations."
  [registry-value]
  (impl.registry/set-default-registry! registry-value))

(defn resolve-registry
  "Resolves supported registry wrapper implementations to a registry map."
  ([]
   (impl.registry/resolve-registry))
  ([registry-value]
   (impl.registry/resolve-registry registry-value)))

(defn register-adapter
  "Registers `adapter` in `registry-value`, with optional `source-id` metadata."
  ([registry-value adapter]
   (impl.registry/register-adapter registry-value adapter))
  ([registry-value adapter source-id]
   (impl.registry/register-adapter registry-value adapter source-id)))

(defn unregister-adapters-by-source
  "Removes adapters from `registry-value` whose registration `source-id` matches."
  [registry-value source-id]
  (impl.registry/unregister-adapters-by-source registry-value source-id))

(defn clear-adapters
  "Removes all adapters from `registry-value`."
  [registry-value]
  (impl.registry/clear-adapters registry-value))

(defn get-adapter
  "Returns the adapter for an API key.

  With one argument, uses the default registry."
  ([api]
   (impl.registry/get-adapter api))
  ([registry-value api]
   (impl.registry/get-adapter registry-value api)))

(defn get-adapters
  "Returns all adapters in the registry.

  With no argument, uses the default registry."
  ([]
   (impl.registry/get-adapters))
  ([registry-value]
   (impl.registry/get-adapters registry-value)))
