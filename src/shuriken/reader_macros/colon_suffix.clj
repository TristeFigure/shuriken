(ns shuriken.reader-macros.colon-suffix
  (:use clojure.pprint)
  (:require [reader-macros.core :refer [set-macro-character lisp-readers]]
            [shuriken.monkey-patch :refer [java-patch
                                           require-from-dependent-namespaces]])
  (:import [clojure.lang LispReader]
           ))

; (set-macro-character \: (fn [reader _colon _ _]
;                           ))

(with-ns clojure)

(java-patch [clojure.lang.LispReader "read"
             [java.io.PushbackReader "boolean" Object "boolean" Object]]
  :after
  "{
       $_ = clojure.lang.RT.list(
         clojure.lang.Symbol.intern(\"clojure.core\", \"from-syntax-quote\"),
         $_
       );
    }")

(reload-)
