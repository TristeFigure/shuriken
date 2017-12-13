(ns shuriken.macro
  (:require [clojure.walk :refer [prewalk walk]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [shuriken.namespace :refer [fully-qualify unqualify]]))

(defn is-form?
  "Determines whether code is a form starting with sym."
  [sym code]
  (boolean (and (seq? code) (-> code first #{sym}))))

(defn wrap-form
  "Wraps code in a form begining with sym unless it is already the case."
  [sym code]
  (if (is-form? sym code)
    code
    `(~sym ~code)))

(defn unwrap-form
  "Unwraps code if is a form starting with sym. Returns code otherwise."
  [sym code]
  (if (is-form? sym code)
    (second code)
    code))

(defn clean-code
  "Recursively unqualifies qualified code in the provided form,
   fully-qualifying back any unqualified symbol with a namespace."
  [code]  
  (prewalk (fn [form]
             (if (symbol? form)
               (as-> (unqualify form) $
                 (if (.getNamespace $)
                   (fully-qualify $)
                   $))
                form))
           code))

; (throw (Exception. "fin"))

; (defmacro lexical-context
;   "Returns the current lexical context as a map from symbols to values.
;   In macros (or anywhere '&env is a local in scope), lexical-context will
;   return the context of the expanded form rather than the context of the macro
;   itself. Use (lexical-context :local true) to override this behavior."
;   [& {:as opts}]
;   (if (and (contains? &env '&env)
;            (not (:local opts)))
;     `(->> (keys ~'&env)
;           (map #(vector `(quote ~%) %))
;           (into {}))
;     `(->> ~(->> (keys &env)
;                 (mapv #(vector `(quote ~%) %)))
;           (filter (comp ~(or (:filter opts) 'identity)
;                         first))
;           (into {}))))

; (defmacro lexical-context
;   "Returns the current lexical context as a map from symbols to values.
;   In macros (or anywhere '&env is a local in scope), lexical-context will
;   return the context of the expanded form rather than the context of the macro
;   itself. Use (lexical-context :local true) to override this behavior."
;   [& {:as opts}]
;   (pprint &form)
;   #_(if (and (contains? &env '&env)
;            (not (:local opts)))
;     `(->> (keys ~'&env)
;           (map #(vector `(quote ~%) %))
;           (into {}))
;     (->> (keys &env)
;          (map #(vector `(quote ~%) %))
;          (into {}))))


; (defmacro m []
;   (lexical-context))

; (defmacro mm []
;   `(lexical-context))

; (let [a 1 b 2 c 3]
;   ; (println "-- fn:"    (lexical-context))
;   ; (println "-- macro:" (m))
;   ; (println "-- macro backtick:" (mm))
;   (println "-- macroexpand:" (macroexpand '(lexical-context))))

; (defmacro store-locals!
;   "Store the current lexical context's locals under a key in memory. Specify
;   pred to filter the locals on the basis of their symbol."
;   ([key]
;    `(store-locals! ~key (constantly true)))
;   ([key pred]
;    `(swap! context assoc ~key
;            (->> (lexical-context (comp ~pred key))))))


; ;; -- Lexical eval
; (defmacro lexical-eval
;   "Evaluate code in the local lexical context."
;   [code]
;   (let [code (->> code
;                   (wrap-form 'quote)
;                   (unwrap-form 'quote))
;         key (str (gensym "lexical-eval-"))]
;     `(try
;        (store-locals! ~key)
;        (eval (binding-context ~key ~code))
;        (finally
;          (delete-context! ~key)))))

; ;; -- File eval
; (def ^:private tmp-files
;   (atom #{})) ;; A set of java file objects

; (defn create-tmp-file! []
;   (let [f (java.io.File/createTempFile "file-eval-" ".clj")]
;     (.deleteOnExit f)
;     (swap! tmp-files conj f)
;     f))

; (defn delete-tmp-file! [f]
;   (.delete f)
;   (swap! tmp-files disj f)
;   (delete-context! (.getAbsolutePath f)))

; (defn clear-tmp-files! []
;   (doseq [f @tmp-files] (delete-tmp-file! f)))

; (defn dump-code [f code]
;   (with-open [w (io/writer f)]
;     (binding
;       [*out* w
;        clojure.pprint/*print-right-margin* 130
;        clojure.pprint/*print-miser-width*  80
;        clojure.pprint/*print-pprint-dispatch* clojure.pprint/code-dispatch]
;       (pprint code))))

; (defn file-eval-error [f ^Throwable t]
;   (let [cause (.getCause t)]
;     (if-not cause
;       (throw t)
;       (throw
;           (doto
;             (Exception. (str "file-eval: caught an exception evaluating code in:"
;                              \newline
;                              (.getAbsolutePath f)
;                              \newline \newline
;                              (-> cause class .getName) " - " (.getMessage cause)))
;             (.setStackTrace (.getStackTrace cause)))))))

; (defmacro file-eval
;   "Evaluate code in a temporary file via load-file in the local lexical
;   context. Keep the temporary file aside if an error is raised, deleting it on
;   the next run."
;   [code]
;   (let [code (->> code
;                   (wrap-form 'quote)
;                   (unwrap-form 'quote))]
;     `(do (clear-tmp-files!)
;          (let [f# (tap (create-tmp-file!)
;                        (-> .getAbsolutePath store-locals!))]
;            (dump-code
;              f# (binding-context ~'*file* ~code)) 
;            (try
;              (tap (load-file (.getAbsolutePath f#))
;                   (delete-tmp-file! f#))
;              (catch Throwable t#
;                (file-eval-error f# t#)))))))

; ;; -- Macroexpand and friends
; (defn macroexpand-some
;   "Recursively macroexpand forms whose first element matches filter in expr.
;   Symbols are passed to filter unqualified."
;   [filter expr]
;   (clojure.walk/prewalk
;     (fn [x]
;       (if (and (seq? x)
;                (filter (or (and (some-> x first symbol?)
;                                 (-> x first unqualify))
;                            (first x))))
;         (macroexpand-1 x)
;         x))
;     expr))

; (defn macroexpand-n
;   "Iteratively call macroexpand-1 on form n times."
;   [n form]
;   (->> (iterate macroexpand-1 form)
;        ;; since the first occurence in the prev lazyseq is form itself, (inc n)
;        (take (inc n))
;        last))

; (defmacro macroexpand-do
;   "(defmacro abc []
;     `(println \"xyz\"))

;   (macroexpand-do (abc))

;   ; -- Macro expansion --
;   ; (clojure.core/println \"xyz\")
;   ;
;   ; -- Running macro --
;   ; xyz

;   or alternatively:

;   (macroexpand-do MODE
;     expr)

;   Where `MODE` has the following meaning:

;   | `MODE`                        | expansion                        |
;   |-------------------------------|----------------------------------|
;   | `nil` (the default)           | `macroexpand`                    |
;   | `:all`                        | `clojure.walk/macroexpand-all`   |
;   | a number n                    | iterate `macroexpand-1` n times  |
;   | anything else                 | a predicate to `macroexpand-some`|"
;   ([expr]
;    `(macroexpand-do nil ~expr))
;   ([mode expr]
;    `(do
;       (println "-- Macro expansion --")
;       (let [mode# ~mode
;             expander# (cond (nil? mode#)   macroexpand
;                             (= :all mode#)  clojure.walk/macroexpand-all
;                             (number? mode#) (partial macroexpand-n    mode#)
;                             :else          (partial macroexpand-some mode#))
;             expansion# (clean-code (expander# (quote ~expr)))]
;         (pprint expansion#)
;         (newline)
        
;         (println "--  Running macro  --")
;         (let [result# (file-eval expansion#)]
;           (pprint result#)
;           (newline)
;           result#)))))
