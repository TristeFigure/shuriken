(ns shuriken.monkey-patch
  "### Tools to monkey-patch Clojure and Java code"
  (:require [clojure.spec.alpha :as s]
            [com.palletops.ns-reload :as nsdeps]
            [shuriken.spec :refer [conf either conform!]]
            [shuriken.namespace :refer [with-ns]]
            [shuriken.macro :refer [is-form? wrap-form unwrap-form]]
            [clojure.repl :refer [source-fn]]
            [shuriken.reflection
             :refer [return-type delegate-name static-method?
                     signature-to-str find-class find-method]])
  (:use robert.hooke)
  (:import RedefineClassAgent))

(defn ^:no-doc prepend-ns
  ([k]
   (prepend-ns *ns* k))
  ([ns k]
   (if (or (symbol? k) (keyword? k))
     (let [ns (or (.getNamespace k) (.getName ns))
           sym (name k)
           f (if (symbol? k) symbol keyword)]
       (f (str ns "/" sym)))
     k)))

(when-not (find-ns 'shuriken.monkey-patch.no-reload-store)
  (let [store-ns (create-ns 'shuriken.monkey-patch.no-reload-store)]
    (intern store-ns 'onlys (atom #{}))))

(defmacro only
  "Ensures `body` is executed only once with respect to `name`.
  If `name` is a symbol or a keyword without a namespace, it will be
  prefixed with the value of `*ns*`.

  ```clojure
  (only 'foo (println \"bar\"))
  ; prints bar

  (only 'foo (println \"bar\"))
  ; prints nothing
  ```"
  [name & body]
  (let [name (if (and (is-form? 'quote name) (-> name second symbol?))
               (->> (unwrap-form 'quote name)
                    prepend-ns
                    (wrap-form 'quote))
               `(prepend-ns ~name))]
    `(let [k# ~name]
       (when-not (contains? @shuriken.monkey-patch.no-reload-store/onlys
                            k#)
         (let [result# ~@body]
           (swap! shuriken.monkey-patch.no-reload-store/onlys
                  conj k#)
           result#)))))

(defn refresh-only
  "Reset `only` statements by name. Next time one is called, the
  associated code will be evaluated.
  If `name` is a symbol or a keyword without a namespace, it will be
  prefixed with `*ns*`.

  ```clojure
  (only 'foo (println \"bar\"))
  ; prints bar

  (refresh-only 'foo)
  (only 'foo (println \"bar\"))
  ; prints bar
  ```"
  [name]
  (swap! shuriken.monkey-patch.no-reload-store/onlys disj (prepend-ns name))
  nil)

(defmacro monkey-patch
  "```clojure
  (monkey-patch perfid-incer clojure.core/+ [original & args]
    (inc (apply original args)))

  (+ 1 2)
  => 4
  ```

  Supports reload. Name and target can be vars or symbols. Name can also be
  a keyword."
  [name target args & body]
  (let [name (-> name clojure.core/name symbol)
        safe-name (wrap-form 'var name)
        safe-target (wrap-form 'var target)]
    `(do
       (if (fn? (deref ~safe-target))
         (do (defn ~name ~args ~@body)
             (add-hook ~safe-target ~safe-name))
         (with-ns '~(-> target .getNamespace symbol)
           (let [~(first args) (deref ~safe-target)]
             (def ~(-> target .getName symbol)
               ~@body)))))))

;; TODO
; (defn copy-class [class-name new-name]
;   (-> (javassist.ClassPool/getDefault)
;       (.getAndRename class-name new-name)))

; (defn method-caller [class-name method]
;   (eval `(fn [& args]
;            (. ~class-name))))

; (def original-method
;   (memoize
;     (fn [method-signature]
;       (let [[class-name method-name parameter-types]
;             (conform! ::method-signature ~method-signature-sym)
;             copy-name (str class-name "Original")
;             original (copy-class class-name copy-name)]
;         (eval `(. original (-> copy-name symbol) ~@args))))))

(defmacro ^:no-doc change-java-method
  [method-signature f & {:keys [once] :or {once false}}]
  (let [method-signature-sym (gensym "method-signature-")
        code `(let [ct-class#   (find-class ~method-signature-sym)
                    was-frozen# (if (.isFrozen ct-class#)
                                  (do (.defrost ct-class#) true)
                                  false)
                    ; copy-name   (str class-name "Original")
                    ; copy        (copy-class class-name copy-name)
                    method#     (find-method ~method-signature-sym)
                    _#          (~f method#)
                    bytecode#   (.toBytecode ct-class#)
                    [class-name# _method-name# _parameter-types#]
                    (conform! :shuriken.reflection/method-signature
                              ~method-signature-sym)
                    definition# (java.lang.instrument.ClassDefinition.
                                  (Class/forName class-name#)
                                  bytecode#)]
                (RedefineClassAgent/redefineClasses
                  (into-array java.lang.instrument.ClassDefinition
                              [definition#]))
                (when was-frozen# (.freeze ct-class#))
                (.stopPruning ct-class# false)
                nil)]
    `(let [~method-signature-sym ~method-signature]
       (if-let [once# ~once]
          (only (str (signature-to-str ~method-signature-sym)
                     "-" once#)
             ~code)
          ~code))))

(def primitive-types
  {"byte"    "Byte"
   "short"   "Short"
   "int"     "Integer"
   "long"    "Long"
   "float"   "Float"
   "double"  "Double"
   "boolean" "Boolean"
   "char"    "Character"})

(defn ^:no-doc cast-str [method-signature expr-str]
  (let [rt (return-type method-signature)
        boxed-t (get primitive-types rt)]
    (if (contains? primitive-types rt)
      (format "(((%s) (%s)).%sValue())" boxed-t expr-str rt)
      (format "((%s) %s)" rt expr-str))))

;; See https://jboss-javassist.github.io/javassist/tutorial/tutorial2.html
;; for the full madness.
(defmulti ^:private javassist-clojure-body #(first %&))

(defmethod javassist-clojure-body :replace [_mode method-signature]
  (format
    "{
      clojure.lang.IFn fn = clojure.java.api.Clojure.var(
        \"%s\", \"%s\"
      );

      return %s;
    }",
    (str *ns*)
    (delegate-name method-signature)
    (cast-str method-signature
              (format "fn.applyTo(%s)"
                      (if (static-method? method-signature)
                        "clojure.lang.RT.seq($args)"
                        "clojure.lang.RT.cons(
                          $0,
                          clojure.lang.RT.seq($args)
                        )")))))

(defmethod javassist-clojure-body :before [_mode method-signature]
  (format
    "{
      clojure.lang.IFn fn = clojure.java.api.Clojure.var(
        \"%s\", \"%s\"
      );

      fn.applyTo(%s);
    }",
    (str *ns*)
    (delegate-name method-signature)
    (if (static-method? method-signature)
      "clojure.lang.RT.seq($args)"
      "clojure.lang.RT.cons(
        $0,
        clojure.lang.RT.seq($args)
      )")))

(defmethod javassist-clojure-body :after [_mode method-signature]
  (let [expr "fn.applyTo(
                clojure.lang.RT.cons($_, clojure.lang.RT.seq($args))
              )"]
    (format
      "{
        clojure.lang.IFn fn = clojure.java.api.Clojure.var(
          \"%s\", \"%s\"
        );

        $_ = %s;
      }",
      (str *ns*)
      (delegate-name method-signature)
      (cast-str
        method-signature
        (if (static-method? method-signature)
          "fn.applyTo(clojure.lang.RT.cons($_, clojure.lang.RT.seq($args)))"
          "fn.applyTo(
            clojure.lang.RT.cons(
              $0,
              clojure.lang.RT.cons($_, clojure.lang.RT.seq($args))
            )
          )")))))

(defn- body-class [body]
  (cond
    (and (= 1 (count body))
         (-> body first string?)) :javassist-body
    :else                         :clojure-body))

(defmulti ^:private emit-java-body #(body-class %4))

(defmethod emit-java-body :javassist-body [_method-signature _mode_ params body]
  (first body))

(defmethod emit-java-body :clojure-body [method-signature mode params body]
  (let [java-body (javassist-clojure-body mode method-signature)]
    `(do (defn- ~(delegate-name method-signature) ~params
           ~@body)
         ~java-body)))

(s/def ::java-patch-args
       (s/cat
         :method-signature any?
         :mode             keyword?
         :params           (s/? vector?)
         :body             (s/+ any?)))

(defmacro java-patch
  "Applies a monkey-patch to a java method.

  `mode` can be either `:replace`, `:before` or `:after`.
  `body` can be javassist pseudo-java (a string) or clojure code.
  In the latter case, `args` must be specified before `body` so as to
  intercept the method's parameters. If `:after` is used `args` will
  include the method's return value. If the method is an instance
  method, `args` will also include the instance it is called on
  (`this`).

  `args`: `[this? return-value? & method-args]`

  ```clojure
  (java-patch [clojure.lang.LispReader \"read\"
               [java.io.PushbackReader \"boolean\" Object \"boolean\" Object]]
    :replace
    [reader eof-is-error eof-value _is-recursive opts]
    (clojure.tools.reader/read
      (merge {:eofthrow eof-is-error
              :eof eof-value}
             opts)
      reader))

  ;; or

  (java-patch [clojure.lang.LispReader$SyntaxQuoteReader \"syntaxQuote\"
               [Object]]
    :after
    \"{
       $_ = clojure.lang.RT.list(
         clojure.lang.Symbol.intern(\"clojure.core\", \"from-syntax-quote\"),
         $_
       );
    }\")
  ```"
  [& args]
  (let [{:keys [method-signature mode params body]}
        (conform! ::java-patch-args args)
        java-body (emit-java-body method-signature mode params body)
        edit (case mode
               :replace '.setBody
               :before '.insertBefore
               :after '.insertAfter)]
    `(change-java-method
       ~method-signature
       (fn [method#]
         (~edit method# ~java-body))
       :once (#{:before :after} ~mode))))

(defn require-from-dependent-namespaces
  "Requires a namespace from any namespace that has required it.
  `requirement` can be anything `require` accepts.

  ```clojure
  (require-from-dependent-namespaces
    '[clojure.core :refer [custom-fn]])
  ```"
  [requirement]
  (let [ns (if (sequential? require)
             (first requirement)
             requirement)]
    (doseq [dep-ns-sym (concat (nsdeps/dependent-namespaces ns)
                               (when (find-ns 'user) '[user]))]
      (eval
        `(with-ns '~dep-ns-sym
           (require '~requirement))))))

(defn define-again
  "Refreshes a var by evaluating its source. Preserves metadata."
  [namespaced-sym]
  (let [ns-str (-> namespaced-sym .getNamespace)
        _ (assert ns-str "Symbol must be namespaced")
        ns-sym (symbol ns-str)
        source (read-string (source-fn namespaced-sym))
        _ (assert source
                  (str "No source found for symbol '" namespaced-sym "'"))
        v (resolve namespaced-sym)]
    (with-ns ns-sym
       (let [orig (meta v)]
         (eval source)
         (reset-meta! v orig)))))
