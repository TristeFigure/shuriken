(ns shuriken.monkey-patches.function-name
  (:use clojure.reflect
        clojure.pprint)
  (:require [shuriken.monkey-patch :refer [java-patch]]))

(java-patch [clojure.lang.Compiler$FnExpr "parse"
             [clojure.lang.Compiler$C clojure.lang.ISeq String]]
  :after
  [return-value context form _name]
  (let [#^clojure.lang.Compiler$ObjExpr expr return-value
        field (.getDeclaredField
                clojure.lang.Compiler$ObjExpr "name")]
    (println "field:" (.name expr))
    (.setAccessible field true)
    (.set field return-value "titix")
    (println "field:" (.name expr))
    (newline))
  (println "return-value:" (.name return-value))
  return-value
  )
