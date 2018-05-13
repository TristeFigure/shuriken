(ns shuriken.reader-macros
  "### Defining reader-macros"
  (:require [shuriken.reflection :refer [read-field static-method]])
  (:import [clojure.lang LispReader]
           [java.io PushbackReader]))

(def reader-macros
  "The java array that stores the reader macros. It's an array used
  as a map of chars to objects (a char being an integer index).
  These object respond to `.invoke` with 4 arguments:
    reader, paragraph, opts & pending-forms."
  (read-field LispReader "macros"))

(defn ^:no-doc ensure-pending [forms]
  (if (nil? forms)
    (new java.util.LinkedList)
    forms))

(defmacro def-reader-macro
  "Defines a reader macro for character `c`. `f` must be a function of
  4 arguments, like `(fn [reader char opts pending-forms] ...)`."
  [c params & body]
  (let [c (str c)
        _ (assert (-> c str count (= 1)))
        f `(fn [reader# char# opts# pending-forms#]
             (let [~params [reader# char# opts#
                            (ensure-pending pending-forms#)]]
               ~@body))]
    `(aset reader-macros (-> ~c seq first) ~f)))

(def read-form
  (static-method
    LispReader "read"
    ;;   reader      eofIsError  eofValue  isRecursive  opts  pendingForms
    [PushbackReader Boolean/TYPE  Object  Boolean/TYPE Object   Object]))
