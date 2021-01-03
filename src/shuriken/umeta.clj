(ns shuriken.umeta
  "### Universal metadata that can hang onto any Object, not just
  implementors of clojure.lang.IMeta"
  (:import java.util.Collections
           clojure.lang.IObj
           WeakIdentityHashMap))

(def clone-method
  (doto (.getDeclaredMethod Object "clone" nil)
        (.setAccessible true)))

(defn clone [obj]
  (.invoke clone-method obj nil))

(defn cloneable? [obj]
  (instance? Cloneable obj))

(def umetas
  (Collections/synchronizedMap (WeakIdentityHashMap.)))

(defprotocol UMetaP
  (umeta [this]
    "Returns the universal metadata of obj, returns nil if there is none.")
  (with-umeta [this ^clojure.lang.IPersistentMap m]
    "Returns an object of the same type and value as obj, with map m
    as its universal metadata."))

(extend-type Object
  UMetaP
    (umeta [this]
      (.get umetas this))
    (with-umeta [this m]
      (let [obj (cond
                  (instance? IObj this)  (with-meta this (into {} (meta this)))
                  (string? this)         (String. this)
                  (cloneable? this)
                  :else                  this)]
        (.put umetas obj m)
        obj)
      (if (instance? IObj this)
        (let [new-obj (with-meta this (meta this))]
          (.put umetas))
        (do (.put umetas this m)
            this))))

(defn vary-umeta
 "Returns an object of the same type and value as obj, with
  (apply f (umeta obj) args) as its universal metadata."
 {:added "1.0"
   :static true}
 [obj f & args]
  (with-meta obj (apply f (meta obj) args)))
