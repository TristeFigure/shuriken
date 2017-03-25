(ns shuriken.macro)

(defn dequote [expr]
  (if (and (list? expr)
           (= 'quote (first expr)))
    (second expr)
    expr))

(defmacro macroexpand-do
  ([expr]
   `(macroexpand-do nil ~expr))
  ([mode expr]
   (let [macro-sym (case mode
                     nil  'macroexpand
                     1    'macroexpand-1
                     :all 'clojure.walk/macroexpand-all)]
     `(do (println "-- Macro expansion -- ")
          (clojure.pprint/pprint (~macro-sym ~expr))
          (newline)
          
          (println "-- Running macro --")
          (let [result# ~(dequote expr)]
            (clojure.pprint/pprint result#)
            (newline)
            result#)))))
