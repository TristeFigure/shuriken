(ns shuriken.macro
  "### Working with macros"
  (:require [clojure.walk :refer [prewalk walk]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [shuriken.namespace :refer [fully-qualify unqualify]]
            [shuriken.context :refer [context! binding-context delete-context!
                                      lexical-context]]
            [shuriken.dance :refer [dance merge-dances]]))

(defn is-form?
  "Determines whether `code` is a form starting with `sym`.
  
  ```clojure
  (is-form? 'a 1)       ; => false
  (is-form? 'a '[a :z]) ; => false
  (is-form? 'a '(a :z)) ; => true
  ```"
  [sym code]
  (boolean (and (seq? code) (-> code first #{sym}))))

(defn wrap-form
  "Wraps `code` in a form begining with `sym` unless it is already
  the case.
  
  ```clojure
  (wrap-form 'a :z)                      ; => (a :z)
  (->> :z (wrap-form 'a) (wrap-form 'a)) ; => (a :z)
  ```"
  [sym code]
  (if (is-form? sym code)
    code
    `(~sym ~code)))

(defn unwrap-form
  "Unwraps `code` if it is a form starting with `sym`. Returns `code`
  otherwise.
  
  ```clojure
  (unwrap-form 'a '(a :z))                        ; a
  (->> '(a :z) (unwrap-form 'a) (unwrap-form 'a)) ; a
  ```"
  [sym code]
  (if (is-form? sym code)
    (second code)
    code))

;; TODO: expose?
(defn- find-fixed-point [f init]
  (let [selection (->> (iterate f init)
                       (partition 2 1)
                       (take-while (fn [[a b]] (not= a b))))]
    [(count selection) (-> selection last second)]))

(defn fully-unwrap-form
  "Recursively calls `unwrap-form` until the form is fully unwrapped."
  [sym code & {:keys [count] :or {count false}}]
  (let [result (find-fixed-point (partial unwrap-form sym)
                                 code)]
    (if count
      result
      (second result))))

(defn clean-code
  "Recursively unqualifies qualified code in the provided form,
   fully-qualifying back any unqualified symbol with a namespace.
  
  ```clojure
  (clean-code `(a (b c)))
  => '(a (b c))
  ```"
  [code]  
  (prewalk (fn [form]
             (if (symbol? form)
               (as-> (unqualify form) $
                 (if (.getNamespace $)
                   (fully-qualify $)
                   $))
                form))
           code))

;; -- File eval
(def ^:private tmp-files
  (atom #{})) ;; A set of java file objects

(defn ^:no-doc create-tmp-file! []
  (let [f (java.io.File/createTempFile "file-eval-" ".clj")]
    (.deleteOnExit f)
    (swap! tmp-files conj f)
    f))

(defn ^:no-doc delete-tmp-file! [f]
  (.delete f)
  (swap! tmp-files disj f)
  (delete-context! (.getAbsolutePath f)))

(defn ^:no-doc clear-tmp-files! []
  (doseq [f @tmp-files] (delete-tmp-file! f)))

(defn ^:no-doc dump-code [f code]
  (with-open [w (io/writer f)]
    (binding
      [*out* w
       clojure.pprint/*print-right-margin* 130
       clojure.pprint/*print-miser-width*  80
       clojure.pprint/*print-pprint-dispatch* clojure.pprint/code-dispatch]
      (pprint code))))

(defn ^:no-doc file-eval-error [f ^Throwable t]
  (let [cause (.getCause t)]
    (if-not cause
      (throw t)
      (throw
          (doto
            (Exception. 
              (str "file-eval: caught an exception evaluating code in:"
                   \newline
                   (.getAbsolutePath f)
                   \newline \newline
                   (-> cause class .getName) " - " (.getMessage cause)))
            (.setStackTrace (.getStackTrace cause)))))))

(defmacro file-eval
  "Evaluate code in a temporary file via load-file in the local
  lexical context. Keep the temporary file aside if an error is
  raised, deleting it on the next run.
  
  ```clojure
  (let [a 1]
    (file-eval '(+ 1 a)))
  ```

  Code evaluated this way will be source-mapped in stacktraces."
  [code]
  (let [code (->> code
                  (wrap-form 'quote)
                  (unwrap-form 'quote))]
    `(do (clear-tmp-files!)
         (let [lc# (lexical-context)
               f# (create-tmp-file!)]
           (context! (.getAbsolutePath f#) lc#)
           (dump-code
             f# (binding-context ~'*file* ~code)) 
           (try
             (let [result# (load-file (.getAbsolutePath f#))]
               (delete-tmp-file! f#)
               result#)
             (catch Throwable t#
               (file-eval-error f# t#)))))))

(defn macroexpand-all-eager
  "Like clojure.walk/macroexpand-all but does not expand quoted forms.
  
  ```clojure
  (defmacro m [] :abc)
  
  (clojure.walk/macroexpand-all '((m) (quote (m))))
  => (:abc (quote :abc))
  
  (macroexpand-all-eager '((m) (quote (m))))
  => (:abc (quote (m)))
  ```"
  [form & {:as opts}]
  (pprint (merge {:walk? #(not (is-form? 'quote %))
                  :pre?  #(and (not (is-form? 'quote %))
                               (seq? %))
                  :pre   macroexpand}
                 opts))
  (dance form :walk? #(not (is-form? 'quote %))
                      :pre?  #(and (not (is-form? 'quote %))
                                   (seq? %))
                      :pre   macroexpand))

(defmacro m [] :abc)
(println "-->" (macroexpand-all-eager '((m) (quote (m)))))

;; -- Macroexpand and friends
(defn macroexpand-some
  "Recursively macroexpand forms whose first element matches filter
  in expr. Symbols are passed to filter unqualified."
  [filter expr]
  (macroexpand-all-eager expr
    :process #(and (seq? %) (filter (or (and (some-> % first symbol?)
                                             (-> % first unqualify))
                                        (first %))))
    :post    macroexpand))

(defn macroexpand-n
  "Iteratively call macroexpand-1 on form n times."
  [n form]
  (->> (iterate macroexpand-1 form)
       ;; since the first occurence in the prev lazyseq is form itself, (inc n)
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

  | `MODE`                        | expansion                        |
  |-------------------------------|----------------------------------|
  | `nil` (the default)           | `macroexpand`                    |
  | `:all`                        | `clojure.walk/macroexpand-all`   |
  | a number n                    | iterate `macroexpand-1` n times  |
  | anything else                 | a predicate to `macroexpand-some`|"
  ([expr]
   `(macroexpand-do nil ~expr))
  ([mode expr]
   `(do
      (println "-- Macro expansion --")
      (let [mode# ~mode
            expander# (cond (nil? mode#)   macroexpand
                            (= :all mode#)  clojure.walk/macroexpand-all
                            (number? mode#) (partial macroexpand-n    mode#)
                            :else          (partial macroexpand-some mode#))
            expansion# (clean-code (expander# (quote ~expr)))]
        (pprint expansion#)
        (newline)
        
        (println "--  Running macro  --")
        (let [result# (file-eval expansion#)]
          (pprint result#)
          (newline)
          result#)))))
