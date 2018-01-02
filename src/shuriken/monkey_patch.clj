(ns shuriken.monkey-patch
  (:require [clojure.spec.alpha :as s]
            [com.palletops.ns-reload :as nsdeps]
            [shuriken.spec :refer [conf either conform!]]
            [shuriken.namespace :refer [with-ns]]
            [shuriken.macro :refer [is-form? wrap-form unwrap-form]]
            [clojure.repl :refer [source-fn]])
  (:use robert.hooke)
  (:import RedefineClassAgent))

;; TODO: document and spec

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
  If `name` is a symbol or a keyword without a namespace, it will be prefixed
  with `*ns*`.
  
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
  "Reset 'only' statements by name. Next time one is called, the associated code
  will be evaluated.
  If `name` is a symbol or a keyword without a namespace, it will be prefixed
  with `*ns*`.
  
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
    (or (class? type) (ct-class? type)) (.getName type)
    :else                               (name type)))

(defn- types-to-str [types]
  (mapv type-to-str types))

(s/def ::method-signature
       (-> (s/cat
             :class-name      (either
                                :string string?
                                :class (conf class? type-to-str)
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
        args (remove nil? [class-name method-name parameter-types])]
    (-> (clojure.string/join "-" args)
        (clojure.string/replace #"\." "_"))))

(defmacro ^:private change-java-method
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

(defmulti ^:private perform-java-patch #(first %&))

(defmethod perform-java-patch :replace [_mode method-signature java-body]
  (change-java-method
    method-signature
    (fn [method]
      (.setBody method java-body))))

(defmethod perform-java-patch :before [_mode method-signature java-body]
  (change-java-method
    method-signature
    (fn [method]
      (.insertBefore method java-body))
    :once true))

(defmethod perform-java-patch :after [_mode method-signature java-body]
  (change-java-method
    method-signature
    (fn [method]
      (.insertAfter method java-body))
    :once true))

(defn- body-class [body]
  (cond
    (and (= 1 (count body))
         (-> body first string?)) :javassist-body
    :else                         :clojure-body))

(defmulti ^:private emit-java-body #(body-class %3))

(defmethod emit-java-body :javassist-body [_method-signature params body]
  (first body))

(defn ^:no-doc javassist-delegate-body [name]
  (format
    "{
       clojure.lang.IFn readFn = clojure.java.api.Clojure.var(
         \"%s\", \"%s\"
       );
    
       return readFn.applyTo(clojure.lang.RT.seq($args));
     }",
    (str *ns*)
    name))

(defmethod emit-java-body :clojure-body [method-signature params body]
  (let [delegate-name (-> method-signature
                          signature-to-str
                          (str "-clojure-delegate")
                          symbol)
        java-body `(javassist-delegate-body delegate-name)]
    `(do (defn- ~delegate-name ~params
           ~@body)
         ~java-body)))

(s/def ::java-patch-args
       (s/cat
         :method-signature any?
         :mode             keyword?
         :params           (s/? vector?)
         :body             (s/+ any?)))

(defmacro java-patch [& args]
   (let [{:keys [method-signature mode params body]}
         (conform! ::java-patch-args args)
         java-body (emit-java-body method-signature params body)]
     `(perform-java-patch ~mode ~method-signature ~java-body)))

(defn require-from-dependent-namespaces [requirement]
  (let [ns (if (sequential? require)
             (first requirement)
             requirement)]
    (doseq [dep-ns-sym (concat (nsdeps/dependent-namespaces ns)
                               (when (find-ns 'user) '[user]))]
      (eval
        `(with-ns '~dep-ns-sym
           (require '~requirement))))))

(defn define-again
  "Refresh a var"
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
