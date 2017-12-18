(ns shuriken.monkey-patch
  (:refer-clojure :exclude [ensure])
  (:require [shuriken.namespace :refer [fully-qualified? with-ns]]
            [shuriken.macro :refer [wrap-form]]
            [clojure.repl])
  (:use robert.hooke))

(defmacro monkey-patch
  "(monkey-patch perfid-incer clojure.core/+ [original & args]
    (inc (apply original args)))
  
  Supports reload. Name and target can be vars or quoted symbols."
  [name target args & body]
  (let [safe-name (wrap-form 'var name)
        safe-target (wrap-form 'var target)]
    `(do
       (if (fn? (deref ~safe-target))
         (do (defn ~name ~args ~@body)
             (add-hook ~safe-target ~safe-name))
         (with-ns '~(-> target .getNamespace symbol)
           (let [~(first args) (deref ~safe-target)]
             (def ~(-> target .getName symbol)
               ~@body)))))))

(defn- prepend-ns
  ([sym]
   (prepend-ns *ns* sym))
  ([ns sym]
   (if (fully-qualified? sym)
     sym
     (symbol (str ns) (str sym)))))

(when-not (let [res (resolve 'onlys)]
            (and res (bound? res)))
  (def onlys
    (atom #{})))

(defmacro only
  "Ensures the code is executed only once with respect to the associated name.
  name must be a symbol, quoted or not."
  [name & body]
  (let [quoted-name (->> (wrap-form 'quote name)
                         eval
                         prepend-ns
                         (wrap-form 'quote))]
    `(do (let [result# (when-not (contains? (deref shuriken.monkey-patch/onlys)
                                            ~quoted-name)
                         ~@body)]
           (swap! onlys conj ~quoted-name)
           result#))))

(defn refresh-only
  "Reset 'only' statements by name. Next time one is called, the associated code
  will be evaluated."
  [name]
  (swap! onlys disj (prepend-ns name))
  nil)
