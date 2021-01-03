(ns shuriken.monkey-patches.additional-definers
  (:require [shuriken.monkey-patch :refer [require-from-dependent-namespaces]]
            [shuriken.namespace :refer [with-ns]]))

(with-ns 'clojure.core
  (defmacro ^:private define-missing-private-definers []
    `(do ~@(for [sym '[def defmacro defmulti defprotocol]
                 :let [new-sym (symbol (str sym "-"))]]
             `(defmacro ~new-sym [name# & more#]
                `(def ~(vary-meta name# assoc :private true) ~@more#)))))

  (define-missing-private-definers))

(require-from-dependent-namespaces
  '[clojure.core :refer [def- defmacro- defmulti- defprotocol-] :reload true])
