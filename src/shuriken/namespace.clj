(ns shuriken.namespace
  "### Namespace related stuff"
  (:require [clojure.string :as str]
            [potemkin :refer [import-vars]]
            [lexikon.core :refer [lexical-eval]]))

;; TODO
; (defn ns-clear
;   "Clears a namespace of its vars, keeping those from clojure.core."
;   ([]
;    (ns-clear *ns*))
;   ([ns]
;    (let [clj-ns (the-ns 'clojure.core)]
;      (->> (ns-map ns)
;           (filter (fn [[_sym v]]
;                     (and (var? v) (-> v .ns (= clj-ns)))))
;           (map (partial ns-unmap ns))
;                                                           ))))

; (defn clear-all-ns []
;   "Strips the current namespace of its vars and deletes all other namespaces
;   except clojure.core."
;   (let [clj-ns (the-ns 'clojure.core)]
;     (doseq [ns (all-ns)
;             :when (not= ns clj-ns)]
;       (if (= *ns* ns)
;         (ns-clear ns)
;         (remove-ns (.getName ns))))))

(defn- class-name [klass]
  (-> (str klass)
      (str/replace #"(?:class|interface) " "")
      symbol))

(defn ns-syms [ns]
  (->> (ns-map *ns*)
       (map (fn [[sym class-or-var]]
              [(if (class? class-or-var)
                 (class-name class-or-var)
                 (let [[ns name] (-> class-or-var meta ((juxt :ns :name)))]
                   (symbol (str ns \/ name))))
               sym]))
       (into {})))

(def fully-qualified-var-regex
  #"^([^/]+)/(.+)$")

(def fully-qualified-class-regex
  #"^(.+)[$.]([^.$]+)$")

(defn parse-fully-qualified-sym [sym]
  (let [s (str sym)]
    (or (re-matches fully-qualified-var-regex s)
        (re-matches fully-qualified-class-regex s))))

(defn- could-be-class-name? [x]
  (let [x (str x)]
    (and (some-> (re-matches #"[^/]*" x)
                 (clojure.string/split #"\.")
                 last
                 seq
                 first
                 Character/isUpperCase)
         x)))

(defn fully-qualify
  "Returns the fully-qualified form of the symbol as if resolved from
  within a namespace.
  Handles namespace aliases.
  `ns`defaults tp `*ns*`.

  (fully-qualified? 'IRecord)      => clojure.lang.IRecord
  (fully-qualified? 'my-var)       => my-ns/my-var
  (fully-qualified? 'alias/my-var) => actual.namespace/my-var"
  ([sym]
   (fully-qualify *ns* sym))
  ([ns sym]
   (if (some-> (.getNamespace sym) could-be-class-name?)
     (symbol (str (-> (.getNamespace sym) symbol fully-qualify)
                  \/
                  (-> sym .getName symbol)))
     (if-let [r (ns-resolve ns sym)]
       (if (class? r)
         (class-name r)
         (symbol (str (-> r meta :ns str)
                      \/
                      (.sym r))))
       sym))))

(defn fully-qualified?
  "Returns true if the symbol constitutes an absolute reference.
  See [[fully-qualified]].

  Handles namespace aliases.
  `ns` defaults to `*ns*`.

  (fully-qualified? 'clojure.lang.IRecord) => true
  (fully-qualified? 'my-ns/my-var)         => true
  (fully-qualified? 'alias/my-var)         => false
  (fully-qualified? '.toString)            => false"
  ([sym]
   (fully-qualified? *ns* sym))
  ([ns sym]
   (boolean
     (and (= (fully-qualify ns sym)
             sym)
          (parse-fully-qualified-sym sym)))))

(defn unqualify
  "Returns the unqualified form of sym.
  Handles namespace aliases.
  `ns` defaults to `*ns*`.

  (unqualifiy 'java.lang.Object)          => Object
  (unqualifiy 'clojure.lang.IRecord)      => clojure.lang.IRecord
  (unqualifiy 'my-ns/my-var)              => my-var
  (unqualifiy 'alias/my-var)              => alias/my-var
  (unqualifiy 'some.path.Class/staticMeth => Class/staticMeth"
  ([sym]
   (unqualify *ns* sym))
  ([ns sym]
   (if (fully-qualified? sym)
     (if (could-be-class-name? sym)
       (if-let [short (get (ns-syms ns) sym)]
         short
         sym)
       (let [[_ ns-part name] (parse-fully-qualified-sym sym)
             shortcut (or (some-> ns-part could-be-class-name?)
                          (-> (->> ns ns-aliases
                                   (map (fn [[sym ns]]
                                          [(-> ns str symbol)
                                           sym]))
                                   (into {}))
                              (get (symbol ns-part))))]
         (if (and shortcut
                  (not (contains? (ns-syms ns) sym)))
           (symbol (str (unqualify (symbol shortcut)) \/ name))
           (symbol name))))
     sym)))

;; Taken and adapted from clojure.contrib
(defmacro with-ns
  "Evaluates body in another namespace. ns is either a namespace
  object or a symbol.  Useful to define functions in namespaces other
  than `*ns*`."
  [ns & body]
  `(binding [*ns* (the-ns ~ns)]
     (lexical-eval (quote (do ~@body)))))

(defmacro import-namespace [ns & {:keys [exclude]}]
  (let [vars (->> (keys (ns-publics (find-ns ns)))
                  (remove (set exclude)))]
    `(import-vars [~ns ~@vars])))
