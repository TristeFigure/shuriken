(ns shuriken.monkey-patches.fn-source
  (:require [shuriken.monkey-patch :refer [monkey-patch]]))

(shuriken.monkey-patch/monkey-patch :add-source-to-fn-meta
  clojure.core/fn
  [original form env & args]
  (let [result (apply original form env args)]
    (vary-meta result assoc :source `(quote ~form))))
