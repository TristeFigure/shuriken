(ns shuriken.read-eval
  (:use clojure.pprint)
  (:require [shuriken.reflection :refer [static-method]])
  (:import [java.io PushbackReader StringReader]))

(defn read-with
  ([reader-class]
   (read-with reader-class *in*))
  ([reader-class stream]
   (read-with reader-class stream true nil))
  ([reader-class stream eof-error? eof-value]
   (read-with reader-class stream eof-error? eof-value false))
  ([reader-class stream eof-error? eof-value recursive?]
   (let [m (static-method
             reader-class
             "read" [PushbackReader Boolean/TYPE Object Boolean/TYPE])]
     (m stream (boolean eof-error?) eof-value recursive?)))
  ([reader-class opts stream]
   (let [m (static-method
             reader-class
             "read" [PushbackReader Object])]
     (m stream opts))))

(defn read-string-with
  [reader-class s & opts]
  (let [stream (PushbackReader. (StringReader. s))]
    (read-with reader-class stream opts)))

(defn eval-with [compiler-class form]
  (let [m (static-method compiler-class "eval" [Object])]
    (m form)))
