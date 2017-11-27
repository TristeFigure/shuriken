(ns shuriken.macro
  (:require [clojure.walk :refer [prewalk walk]]
            [shuriken.namespace :refer [unqualify]]))

(defn clean-code
  "Recursively unqualifies qualified code in the provided form."
  [code]
  (prewalk #(if (symbol? %) (unqualify %) %)
           code))

(defn macroexpand-some
  "Recursively macroexpand forms whose first element matches filter in expr.
  Symbols are passed to filter unqualified."
  [filter expr]
  (let [process (fn process [x]
                  (if (and (seq? x)
                           (filter (or (and (some-> x first symbol?)
                                            (-> x first unqualify))
                                       (first x))))
                    (macroexpand-some filter (macroexpand-1 x))
                    x))]
    (walk
      process
      identity
      (process expr))))

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

  Where `MODE` is one of:

  | `MODE`                        | expansion                       |
  |-------------------------------|---------------------------------|
  | `nil` (the default)           | `macroexpand`                   |
  | `:all`                        | `clojure.walk/macroexpand-all`  |
  | a number n                    | iterate `macroexpand-1` n times |
  | a (seq of) symbol(s) or a ifn | `macroexpand-some`              |"
  ([expr]
   `(macroexpand-do nil ~expr))
  ([mode expr]
   (let [expander
         (cond
           (nil? mode)    'macroexpand
           (number? mode) `(fn [expr#]
                             (->> (iterate macroexpand-1 expr#)
                                  (take ~mode)
                                  last))
           (= :all mode)  'clojure.walk/macroexpand-all
           
           (or (symbol? mode) (coll? mode))
           `(partial macroexpand-some
                     ~(cond
                        (symbol? mode) `'#{~mode}
                        (list? mode)    mode
                        :else          `(set '~mode)))
           
           :else (throw (IllegalArgumentException.
                          (str "Invalid mode " mode))))]
     `(do (println "-- Macro expansion --")
          (clojure.pprint/pprint
            (clean-code (~expander (quote ~expr))))
          (newline)
          
          (println "--  Running macro  --")
          (let [result# ~expr]
            (clojure.pprint/pprint result#)
            (newline)
            result#)))))
