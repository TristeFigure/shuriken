(ns shuriken.macro)

(defmacro macroexpand-do
  ([mode expr]
   (let [macro-sym (case mode
                     0    'macroexpand
                     1    'macroexpand-1
                     :all 'clojure.walk/macroexpand-all)]
     `(do (println "-- Macro expansion -- ")
          (clojure.pprint/pprint
            (~macro-sym '~expr))
          (newline)
          (println "-- Running macro --")
          ~expr
          (newline)))))
