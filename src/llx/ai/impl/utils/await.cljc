(ns llx.ai.impl.utils.await
  (:require
   #?@(:clj [[clojure.main :as main]])
   [promesa.core :as p]))

(defn- root-cause
  [t]
  #?(:clj (main/root-cause t)
     :cljs t))

(defn await!
  ([x]
   (if (p/deferred? x)
     (let [result (p/await x)]
       (if (instance? #?(:clj Throwable :cljs js/Error) result)
         (throw (root-cause result))
         result))
     x))
  ([x duration default-on-timeout]
   (if (p/deferred? x)
     (let [result (p/await x duration default-on-timeout)]
       (if (instance? #?(:clj Throwable :cljs js/Error) result)
         (throw (root-cause result))
         result))
     x)))
