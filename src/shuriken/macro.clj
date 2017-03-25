(ns shuriken.macro)

(defmacro macroexpand-do
  ([expr]
   `(macroexpand-do nil ~expr))
  ([mode expr]
   (let [macro-sym (case mode
                     nil  'macroexpand
                     1    'macroexpand-1
                     :all 'clojure.walk/macroexpand-all)]
     `(do (println "-- Macro expansion -- " ~expr)
          (clojure.pprint/pprint (~macro-sym (quote ~expr)))
          (newline)
          
          (println "-- Running macro --")
          (let [result# ~expr]
            (clojure.pprint/pprint result#)
            (newline)
            result#)))))
