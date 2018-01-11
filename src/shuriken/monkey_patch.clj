(ns shuriken.monkey-patch
  "### Tools to monkey-patch Clojure and Java code"
  (:require [clojure.spec.alpha :as s]
            [com.palletops.ns-reload :as nsdeps]
            [shuriken.spec :refer [conf either conform!]]
            [shuriken.namespace :refer [with-ns]]
            [shuriken.macro :refer [is-form? wrap-form unwrap-form]]
            [clojure.repl :refer [source-fn]])
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
  prefixed with `*ns*`.
  
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
  
  Supports reload. Name and target can be vars or quoted symbols."
  [name target args & body]
  (let [safe-name (wrap-form 'var name)
        safe-target (wrap-form 'var target)]
    `(do
       (if (fn? (deref ~safe-target))
         (do (defn ~name ~args ~@body)
             (add-hook ~safe-target ~safe-name))
         (with-ns '~(-> target .getNamespace symbol)
           (let [~(first args) (deref ~safe-target)]
             (def ~(-> target .getName symbol)
               ~@body)))))))

(defn- ct-class? [x]
  (instance? javassist.CtClass x))

(defn- type-to-str [type]
  (cond
    (string? type)                      type
    (symbol? type)                       (str type)
    (or (class? type) (ct-class? type)) (.getName type)
    :else                               (name type)))

(defn ^:no-doc types-to-str [types]
  (mapv type-to-str types))

(s/def ::method-signature
       (-> (s/cat
             :class-name      (either
                                :string (conf string? identity str)
                                :class  (conf class? type-to-str)
                                :symbol (conf symbol? type-to-str)
                                :unform :string)
             :method-name     (conf #(or (string? %) (ident? %))
                                    name)
             :parameter-types (s/?
                                (conf (s/coll-of
                                        #(or (string? %) (ident? %) (class? %)))
                                      types-to-str)))
           (conf (comp vec (juxt :class-name :method-name :parameter-types))
                 #(zipmap % [:class-name :method-name :parameter-types]))))

(defn ^:no-doc signature-to-str [method-signature]
  (let [[class-name method-name parameter-types]
        (conform! ::method-signature method-signature)
        args (remove nil? (concat [class-name method-name]
                                  parameter-types))]
    (-> (clojure.string/join "-" args)
        (clojure.string/replace #"\." "_"))))

(defmacro ^:no-doc change-java-method
  [method-signature f & {:keys [once] :or {once false}}]
  (let [method-signature-sym (gensym "method-signature-")
        code `(let [[class-name# method-name# parameter-types#]
                    (conform! ::method-signature ~method-signature-sym)
                    class-pool# (javassist.ClassPool/getDefault)
                    ct-class#   (doto
                                  (.get class-pool# class-name#)
                                  (.stopPruning true))
                    was-frozen# (if (.isFrozen ct-class#)
                                  (do (.defrost ct-class#) true)
                                  false)
                    method#
                    (->> (.getMethods ct-class#)
                         (filter
                           #(and (= method-name# (.getName %))
                                 (if (nil? parameter-types#)
                                       true
                                       (= (-> % .getParameterTypes types-to-str)
                                          parameter-types#))))
                         first)
                    _# (~f method#)
                    bytecode# (.toBytecode ct-class#)
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
       ~(if once
          `(only (signature-to-str ~method-signature-sym)
             ~code)
          code))))

;; See https://jboss-javassist.github.io/javassist/tutorial/tutorial2.html
;; for the full madness.
(defmulti ^:private javassist-clojure-body #(first %&))

(defmethod javassist-clojure-body :replace [_mode name]
  (format
    "{
      clojure.lang.IFn fn = clojure.java.api.Clojure.var(
        \"%s\", \"%s\"
      );
      
      return fn.applyTo(clojure.lang.RT.seq($args));
    }",
    (str *ns*)
    name))

(defmethod javassist-clojure-body :before [_mode name]
  (format
    "{
      clojure.lang.IFn fn = clojure.java.api.Clojure.var(
        \"%s\", \"%s\"
      );
      
      
      fn.applyTo(clojure.lang.RT.seq($args));
    }",
    (str *ns*)
    name))

(defmethod javassist-clojure-body :after [_mode name]
  (format
    "{
      clojure.lang.IFn fn = clojure.java.api.Clojure.var(
        \"%s\", \"%s\"
      );
      
      $_ = fn.applyTo(
        clojure.lang.RT.cons($_, clojure.lang.RT.seq($args))
      );
    }",
    (str *ns*)
    name))

(defn- body-class [body]
  (cond
    (and (= 1 (count body))
         (-> body first string?)) :javassist-body
    :else                         :clojure-body))

(defmulti ^:private emit-java-body #(body-class %4))

(defmethod emit-java-body :javassist-body [method-signature mode params body]
  (first body))

(defmethod emit-java-body :clojure-body [method-signature mode params body]
  (let [delegate-name (-> method-signature
                          signature-to-str
                          (str "-clojure-delegate")
                          symbol)
        java-body (javassist-clojure-body mode delegate-name)]
    `(do (defn- ~delegate-name ~params
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
   
  `mode` can be either :replace, :before or :after  
  `body` can be javassist pseudo-java or clojure code. In case of the
  latter, `args` must be specified before `body` so as to intercept
  the method's parameters. If `:after` is used the first argument will
  be the method's return value.
  
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
       ; :once (#{:before :after} mode)
       )))

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

(defn static-field
  "Returns the value of a static field by reflection."
  [class name]
  (-> (doto (.getDeclaredField class (str name))
        (.setAccessible true))
      (.get nil)))

(defn method
  "Returns a method by reflection."
  [class name parameter-types]
  (let [m (-> (doto (.getDeclaredMethod class (str name)
                                        (into-array Class parameter-types))
                (.setAccessible true)))]
    (fn [target & args]
      (.invoke m target (to-array args)))))

(defn static-method
  "Returns a static method by reflection."
  [class name parameter-types]
  (let [m (method class name parameter-types)]
    (partial m nil)))
