(ns shuriken.macro
  (:require [clojure.walk :refer [prewalk]]
            [shuriken.namespace :refer [unqualify]]))

(defn clean-code
  "Recursively unqualifies qualified code in the provided form."
  [code]
  (prewalk #(if (symbol? %) (unqualify %) %)
           code))

(defmacro macroexpand-do
  "(defmacro abc []
    `(println \"xyz\"))

  (macroexpand-do (abc))

  ; -- Macro expansion --
  ; (clojure.core/println \"xyz\")
  ;
  ; -- Running macro --
  ; xyz

  or alternatively:

  (macroexpand-do MODE
    expr)

  Where MODE is one of:

  |-------------------------------------|
  | nil  | macroexpand (the default)    |
  | 1    | macroexpand-1                |
  | :all | clojure.walk/macroexpand-all |
  |-------------------------------------|"
  ([expr]
   `(macroexpand-do nil ~expr))
  ([mode expr]
   (let [macro-sym (case mode
                     nil  'macroexpand
                     1    'macroexpand-1
                     :all 'clojure.walk/macroexpand-all)]
     `(do (println "-- Macro expansion --")
          (clojure.pprint/pprint
            (clean-code (~macro-sym (quote ~expr))))
          (newline)
          
          (println "--  Running macro  --")
          (let [result# ~expr]
            (clojure.pprint/pprint result#)
            (newline)
            result#)))))
