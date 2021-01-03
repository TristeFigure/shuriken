(ns shuriken.reader-macros.diaeresis-unquote
  (:use clojure.pprint)
  (:require [reader-macros.core :refer [set-macro-character
                                        macro-read-unquote]]))

(def diaeresis-unquotations
  (atom {}))

(def ^:dynamic *store-unquote*)
(def ^:dynamic *store-unquote*)

(defn mem-unquote
  ([expr & {:keys [unquoter genkey store retrieve]
            :or {unquoter 'clojure.core/unquote
                 genkey   #(do `(quote ~(gensym "diaeresis-unquotation-")))
                 store    `(partial swap! diaeresis-unquotations assoc)
                 retrieve `(partial get @diaeresis-unquotations)}}]
   (let [key (genkey)]
     ``(~'~unquoter (do (~'~store '~~key ~'~expr)
                        '(~'~retrieve '~~key))))))

(defmacro at-mexp [& body]
  (eval '(do ~@body)))

(defmacro mem-unquote-splicing [expr]
  `(mem-unquote ~expr :unquoter clojure.core/unquote-splicing))

(defn diaeresis-reader [reader double-unquote opts pending-forms]
  (let [[unquoter expr] (macro-read-unquote reader \~ opts pending-forms)]
    `(mem-unquote ~expr :unquoter ~unquoter)))

(set-macro-character \¨ diaeresis-reader)

(defmacro xm [a]
  (let [code `~(mem-unquote a)]
    code))

(pprint (macroexpand '(xm 1)))
