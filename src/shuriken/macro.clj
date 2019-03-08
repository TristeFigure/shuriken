(ns shuriken.macro
  "### Tools for building macros"
  (:require [clojure.walk :refer [prewalk]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint] :as pprint]
            [clojure.spec.alpha :as s]
            [lexikon.core :refer [lexical-context lexical-eval lexical-resolve]]
            [dance.core :refer [defdance dance depth-dance]]
            [shuriken.meta :refer [preserve-meta]]
            [shuriken.namespace :refer [fully-qualify unqualify]]
            [shuriken.spec :refer [conform!]])
  (:import [java.io File]))

;; TODO: stronger file-eval by preventing reload-based initialisation using the
;; `only` macro.

(defn macro?
  ([x]
   (macro? *ns* x))
  ([reference-ns x]
    (if (var? x)
      (-> x meta :macro)
      (->> x (ns-resolve reference-ns) meta :macro))))

(defn is-form?
  "Determines whether `code` is a form starting with `sym`.

  ```clojure
  (is-form? 'a 1)       ; => false
  (is-form? 'a '[a :z]) ; => false
  (is-form? 'a '(a :z)) ; => true
  ```"
  [sym code]
  (boolean (and (seq? code) (-> code first #{sym}))))

;; TODO: rename wrap-form & all to wrap & wrap-all etc..
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
  "Recursively calls [[unwrap-form]] until the form is fully unwrapped."
  [sym code & {:keys [count] :or {count false}}]
  (let [result (find-fixed-point (partial unwrap-form sym)
                                 code)]
    (if count
      result
      (second result))))

;; TODO: use dance to not unqualify data.
(defn clean-code
  "Unqualifies any symbol in `code`, and fully-qualifies those with an
  aliased namespace.

  ```clojure
  (clean-code `(a (b c)))
  => '(a (b c))

  By default `reference-ns` is *ns*.
  ```"
  ([code]
   (clean-code *ns* code))
  ([reference-ns code]
   (prewalk (fn [form]
              (if (and (symbol? form)
                       (or (contains?
                             #{"clojure.core" (str (.getName reference-ns))}
                             (.getNamespace form))
                           (contains?
                             (ns-aliases reference-ns)
                             (symbol (str (.getNamespace form))))
                           (contains?
                             (->> reference-ns ns-imports vals (map pr-str) set)
                             (str form))))
                (as-> (unqualify reference-ns form) $
                  (if (.getNamespace $)
                    (fully-qualify reference-ns $)
                    $))
                form))
            code)))

;; TODO: upgrade dance and remove.
(let [not-quote-form? #(not (is-form? 'quote %))]
  (defdance reluctant-macroexpanding-dance
    :pre #(if (not-quote-form? %)
            (macroexpand %)
            %)
    :walk? not-quote-form?))

;; -- File eval
;; TODO: Windows ?
(defn macroexpand-all-code
  ;; TODO: document opts
  "Like clojure.walk/macroexpand-all but does not expand quoted forms.

  ```clojure
  (defmacro m [] :abc)

  (clojure.walk/macroexpand-all '((m) (quote (m))))
  => (:abc (quote :abc))

  (macroexpand-all-code '((m) (quote (m))))
  => (:abc (quote (m)))
  ```"
  [expr & {:as opts}]
  (dance expr reluctant-macroexpanding-dance opts))

(def ^:dynamic *macroexpansions-dir*
  (io/file "./src/macroexpansions"))

(def ^:private expansion-files
  (atom {})) ;; A map from full macroexpansions to expansion files

;; TODO: move to shuriken ?
(defn- relative-path
  ([f]       (relative-path f "."))
  ([f ref-f] (let [f (io/file f)
                   ref-f (io/file ref-f)]
               (.. ref-f
                   toURI
                   (relativize (.. f toURI))
                   toString))))

(defn- parent-dirs [f]
  (->> (iterate (memfn ^java.io.File getParentFile) f)
       (take-while identity)))

(defn- expansion-dir [file]
  (io/file *macroexpansions-dir* (relative-path file)))

;; TODO: support .cljs, .cljc etc
(defn- expansion-file [code line column file]
  (java.io.File. (expansion-dir file)
                 (-> (str "L" line ":" "C" column " - "
                          (binding [*print-length* 3 *print-level* 2]
                            (pr-str (clean-code code)))
                          ".clj")
                     (str/replace (File/separator) "_SLASH_"))))

(defn code-signature [line column file]
  (merge {:line -1 :column -1 :file (-> *file* io/file relative-path)}
         {:line line :column column :file file}))

(defn file-for-code [code line column file]
  (let [signature (code-signature line column file)]
    (or (@expansion-files signature)
        (let [f (expansion-file code line column file)]
          (swap! expansion-files assoc signature f)
          f))))

(defn- unmkdirs [f]
  (let [mxpd-pth (.getCanonicalPath (io/file *macroexpansions-dir*))
        dirs (take-while #(not= mxpd-pth (.getCanonicalPath %))
                         (parent-dirs f))]
    (doseq [d dirs
            :let [d-pth (.getCanonicalPath d)]
            :while (empty? (.listFiles d))]
      (when (.exists d) (.delete d)))))

(defn delete-expansion-file! [signature]
  (let [f (get @expansion-files signature)]
    (when (.exists f) (.delete f))
    (swap! expansion-files dissoc signature)
    (unmkdirs f)))

(defn- ensure-file [f]
  (.mkdirs (.getParentFile f))
  (.createNewFile f))

(defn dump-code [f code]
  (let [f (io/file f)]
    (ensure-file f)
    (with-open [w (io/writer f)]
      (binding [*out*                          w
                pprint/*print-right-margin*    130
                pprint/*print-miser-width*     80
                pprint/*print-pprint-dispatch* pprint/code-dispatch]
        (pprint code)))))

(defn file-eval-error [f ^Throwable t]
  (let [cause (.getCause t)]
    (if (and cause (-> cause ex-data :type (not= :file-eval-error)))
      (doto
        (ex-info
          (str "file-eval: caught an exception evaluating code in:"
               \newline
               (.getCanonicalPath f)
               \newline \newline
               (-> cause class .getName) " - " (.getMessage cause))
          {:type :file-eval-error
           :cause cause})
        (.setStackTrace (.getStackTrace cause)))
      t)))


;; Taken from weaving.core
(defn- || [f & args]
  #((apply partial f (concat args %&))))

;; TODO: document the fact that if the expansion is the same the file is not
;; overwritten, allowing for manual tweaking of the expansion to determine
;; what's wrong.

;; TODO: make private
(defmacro eval-file-delete [sign f]
  `(let [f# ~f]
     (try
       (let [result# (-> f# slurp read-string lexical-eval)]
         (delete-expansion-file! ~sign)
         result#)
       (catch Throwable t#
         (throw (file-eval-error f# t#))))))

;; TODO: doesn't handle ` well. (file-eval `(a-macro)) does not work like eval.
(defn- file-eval* [code & {:keys [locals line column file surface-code]}]
  (let [code-expr (cond (symbol? code) `(lexical-resolve ~locals '~code)
                        (seq? code)    (wrap-form 'quote code)
                        :else          code)]
    `(let [f#    (file-for-code
                   (vary-meta '~surface-code merge ~{:line line :column
                                                     column :file file})
                   ~line ~column ~file)
           sign# (code-signature ~line ~column ~file)]
       (dump-code f# ~code-expr)
       (eval-file-delete sign# f#))))

(defmacro file-eval
  "Evaluate code in a temporary file via `load-file` in the local
  lexical context. Keep the temporary file aside if an error is
  raised, deleting it on the next run.

  ```clojure
  (let [a 1]
  (file-eval '(+ 1 a)))
  ```

  Code evaluated this way will be source-mapped in stacktraces."
  [code & {:keys [locals line column file surface-code]}]
  (file-eval* code
              :locals       (or locals       (lexical-context))
              :line         (or line         (:line   (meta &form)))
              :column       (or column       (:column (meta &form)))
              :file         (or file         *file*)
              :surface-code (or surface-code code)))

;; -- Macroexpand and friends
;; TODO: document opts
(defn macroexpand-some
  "Recursively macroexpands seqs in `expr` whose first element matches
  `pred`. Symbols are passed to `filter`, their namespace stripped.
  Quoted forms are not expanded.

  ```clojure
  (macroexpand-some #{'let}
    '(let [a (let [aa 1] aa)
           b '(let [bb 2] bb)]
       (dosync [a b])))
  => (let* [a (let* [aa 1] aa)  ; expanded
            b '(let [bb 2] bb)] ; not expanded since quoted
       (dosync nil))            ; not expanded b/c pred does not match
  ```"
  [pred expr & {:as opts}]
  (dance expr
         reluctant-macroexpanding-dance
         {:pre? (fn [x]
                  (when (seq? x)
                    (let [sym (or (and (some-> x first symbol?)
                                       (-> x first name symbol))
                                  (first x))]
                      (pred sym))))}
         opts))

(defn macroexpand-n
  "Iteratively call `macroexpand-1` on `form` `n` times."
  [n form]
  (->> (iterate macroexpand-1 form)
       ;; since the first occurence in the prev lazyseq is form itself, (inc n)
       (take (inc n))
       last))

;; TODO: document use of macroexpand-all-code or change it.
(defn macroexpand-depth
  "Expands the first `n` levels of `expr` with
  [[macroexpand-all-code]]."
  [n expr]
  (dance expr
         reluctant-macroexpanding-dance
         depth-dance
         :walk? (fn [form ctx] [(< (:depth ctx) n) ctx])))

(s/def ::macroexpand-do-args
       (s/cat
         :mode     (s/? any?)
         :mode-arg (s/? any?)
         :expr     any?))

; (def idempotent-file-eval*
;   (idempotent (fn [env code]
;                 `(file-eval ~env ~code))
;               (fn [_expanding-code prev-expansion new-expansion env _code]
;                 (let [f (-> prev-expansion expansion-file)]
;                   (if (.exists f)
;                     `(eval-file-delete ~(code-signature prev-expansion) ~f)
;                     `(file-eval ~env ~new-expansion))))))

; (def ^:private file-eval-idempotencies
;   (atom {}))

; (defmacro idempotent-file-eval [expanding-code expansion]
;   `(let [prev-expansion# (get @file-eval-idempotencies '~expanding-code)
;          f# (some-> prev-expansion# expansion-file)]
;      (if (and prev-expansion# (.exists f#))
;        (eval-file-delete (code-signature prev-expansion#) f#)
;        (file-eval ~(compile-and-runtime-locals) expansion~))))

; TODO: find a way to provide reluctant code expansion as a global opt
; such as a binding.
(defmacro ^:no-doc macroexpand-do* [expander expr line column file]
  (let [expanding-code (concat expander [`'~expr])]
    `(let [expansion# (file-eval ~expanding-code
                                 :line ~line :column ~column :file ~file)]
       (do
         (pprint expansion#)
         (newline)

         (println "--  Running macro  --")
         (let [result# (file-eval expansion#
                                  :line ~line :column ~column :file ~file
                                  :surface-code ~expr)]
           (pprint result#)
           (newline)
           result#)))))

;; TODO: rewrite doc to account for use of macroexpand-all-code
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
  | `:all`                        | `macroexpand-all-code`           |
  | :n  <number>                  | `macroexpand-n`                  |
  | :depth <number>               | `macroexpand-depth`              |
  | :some <fn>                    | `macroexpand-some`               |"
  [& args]
  (let [expr (last args)]
    (if-not (coll? expr)
      expr
      (let [{:keys [mode mode-arg expr]} (conform! ::macroexpand-do-args args)
            expr (-> expr
                     (->> (wrap-form 'quote)
                          (unwrap-form 'quote))
                     (as-> $ (preserve-meta $ (clean-code $))))
            {:keys [line column]} (meta expr)
            expander (case mode
                       nil    `(macroexpand-depth 1)
                       :all   `(macroexpand-all-code)
                       :n     `(macroexpand-n     ~mode-arg)
                       :depth `(macroexpand-depth ~mode-arg)
                       :some  `(#(macroexpand-some
                                   ~mode-arg
                                   %
                                   reluctant-macroexpanding-dance)))]
        (println "-- Macro expansion --")
        `(macroexpand-do* ~expander ~expr ~line ~column *file*)))))
