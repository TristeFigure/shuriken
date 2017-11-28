(ns shuriken.macro
  (:require [clojure.walk :refer [prewalk walk]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [shuriken.namespace :refer [unqualify]]
            [shuriken.threading :refer [tap]]))

(defn clean-code
  "Recursively unqualifies qualified code in the provided form."
  [code]
  (prewalk #(if (symbol? %) (unqualify %) %)
           code))

(def ^:private tmp-files
  (atom []))

(defn- create-tmp-file! []
  (let [f (java.io.File/createTempFile "macroexpand-do-" ".clj")]
    (.deleteOnExit f)
    (swap! tmp-files conj f)
    f))

(defn- clear-tmp-files! []
  (swap! tmp-files
         (fn [fs]
           (doseq [f fs] (.delete f))
           [])))

(defn file-eval
  "Evaluate file in a temporary file via load-file."
  [code]
  (clear-tmp-files!)
  (let [f (create-tmp-file!)]
    (with-open [w (io/writer f)]
      (binding [*out* w]
        (pprint code)))
    (try
      (tap (load-file (.getCanonicalPath f))
           (.delete f))
      (catch Throwable t
        (throw (Exception.
                 (str "file-eval: caught an exception evaluating code in:"
                      \newline
                      (.getCanonicalPath f))
                 t))))))

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

(defn macroexpand-n
  "Iteratively call macroexpand-1 on form n times."
  [n form]
  (->> (iterate macroexpand-1 form)
       ;; since the first occurence in the next lazyseq is form itself, (inc n)
       (take (inc n))
       last))

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

  Where `MODE` has the following meaning:

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
           (= :all mode)  'clojure.walk/macroexpand-all
           (number? mode) 'macroexpand-n
           
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
          (let [result# (file-eval '~expr)]
            (clojure.pprint/pprint result#)
            (newline)
            result#)))))
