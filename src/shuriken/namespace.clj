(ns shuriken.namespace
  "### Namespace related stuff"
  (:use clojure.pprint)
  (:require [clojure.string :as str]
            [potemkin :refer [import-vars]]
            [shuriken.context :refer [lexical-eval]]))

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

(def fully-qualified-regex
  ;; ns   . or /  name  eol
  #"(^.*)(?:/|\.)([^./]+)$")

(defn- could-be-class-name? [x]
  (let [x (str x)]
    (and (some-> x
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
     (if-let [r (ns-resolve *ns* sym)]
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
  (fully-qualified? 'alias/my-var)         => false"
  ([sym]
   (fully-qualified? *ns* sym))
  ([ns sym]
   (boolean
     (and (= (fully-qualify ns sym)
             sym)
          (re-matches fully-qualified-regex (str sym))))))

(defn unqualify
  "Returns the unqualified form of sym.
  Handles namespace aliases.
  `ns` defaults to `*ns*`.

  (unqualifiy 'clojure.lang.IRecord)      => IRecord
  (unqualifiy 'my-ns/my-var)              => my-var
  (unqualifiy 'alias/my-var)              => alias/my-var
  (unqualifiy 'some.path.Class/staticMeth => Class/staticMeth"
  ([sym]
   (unqualify *ns* sym))
  ([ns sym]
   (if (fully-qualified? sym)
     (let [[_ ns-part name] (re-matches fully-qualified-regex (str sym))
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
         (symbol name)))
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
