(ns shuriken.reader-macros.apex-quote
  (:use clojure.pprint)
  (:require [reader-macros.core :refer [set-macro-character lisp-readers]]
            [clojure.tools.reader :as rd]
            [shuriken.monkey-patch :refer [defcopy access-private-vars|]])
  (:import [clojure.lang LispReader IRecord]
           [java.util regex.Pattern]))

(def ^:private register-gensym @#'rd/register-gensym)
(def ^:private flatten-map @#'rd/flatten-map)
(def ^:private syntax-quote-coll @#'rd/syntax-quote-coll)
(def ^:private add-meta @#'rd/add-meta)
(def ^:private unquote? @#'rd/unquote?)
(def ^:private unquote-splicing? @#'rd/unquote-splicing?)

(def ^:private ^:dynamic gensym-env nil)

(pprint
  (macroexpand '(defcopy 'rd/syntax-quote*
    (access-private-vars| 'clojure.tools.reader
                          #{#'rd/add-meta
                            #'rd/register-gensym
                            #'rd/unquote?
                            #'rd/unquote-splicing?
                            #'rd/syntax-quote-coll
                            #'rd/flatten-map}))))
; (defcopy 'rd/syntax-quote)

(throw (Exception. "fin"))

(defn- apex-quote* [form]
  (->>
   (cond
    (special-symbol? form) (list 'quote form)

    (symbol? form)
    (list 'quote
          (if (namespace form)
            (let [maybe-class ((ns-map *ns*)
                               (symbol (namespace form)))]
              (if (class? maybe-class)
                (symbol (.getName ^Class maybe-class) (name form))
                (rd/resolve-symbol form)))
            (let [sym (str form)]
              (cond
               (.endsWith sym "#")
               (register-gensym form)

               (.startsWith sym ".")
               form

               :else (rd/resolve-symbol form)))))

    (unquote? form) (second form)
    (unquote-splicing? form) (throw (IllegalStateException. "unquote-splice not in list"))

    (coll? form)
    (cond

     (instance? IRecord form) form
     (map? form) (syntax-quote-coll (rd/map-func form) (flatten-map form))
     (vector? form) (list 'clojure.core/vec (syntax-quote-coll nil form))
     (set? form) (syntax-quote-coll 'clojure.core/hash-set form)
     (or (seq? form) (list? form))
     (let [seq (seq form)]
       (if seq
         (syntax-quote-coll nil seq)
         '(clojure.core/list)))

     :else (throw (UnsupportedOperationException. "Unknown Collection type")))

    (or (keyword? form)
        (number? form)
        (char? form)
        (string? form)
        (nil? form)
        (instance? Boolean form)
        (instance? Pattern form))
    form

    :else (list 'quote form))
   (add-meta form)))


(defmacro apex-quote
  "Macro equivalent to the syntax-quote reader macro (`)."
  [form]
  (binding [gensym-env {}]
    (apex-quote* form)))

(println "---->" (apex-quote '(abc ~(+ 1 (eval `(+ 1 ~(inc 2)))))))

(defn reader-macros []
  (set (keys lisp-readers)))

(defn read-chars-while [char-pred reader]
  (loop [r reader acc []]
    (let [c (char (.read reader))]
      (if (char-pred c)
        (recur reader (conj acc c))
        (do (.unread reader (int c))
            acc)))))

(defn- whitespace? [ch]
  (or (Character/isWhitespace ch)
      (= ch \,)))

(defn read-opts-chars [reader]
  (set (read-chars-while
         #(and (not (reader-macros %))
               (not (whitespace? %)))
         reader)))

(defn apex-reader [reader apex opts pending-forms]
  (let [reader-macros (reader-macros)
        opts-chars (set (read-chars-while #(not (contains? reader-macros %))
                                    reader))]
    (println "reader" (read reader))))

(set-macro-character \´ apex-reader)

(defn diaeresis-reader [reader diaeresis opts pending-forms]
  (let [reader-macros (reader-macros)
        opts-chars (set (read-chars-while
                          #(and (not (reader-macros %))
                                (not (whitespace? %)))
                          reader))]
    ))

(set-macro-character \´ diaeresis-reader)

(println ´[1 2 3])
