(ns shuriken.namespace)

(defn class-name [klass]
  (-> (str klass)
      (clojure.string/replace #"(?:class|interface) " "")
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

(defn fully-qualify
  "Returns a fully-qualified symbol as if resolved from within a namespace.
  Handles namespace aliases. Default namespace is *ns*.

  (fully-qualified? 'IRecord)       => clojure.lang.IRecord
  (fully-qualified? 'my-var)        => my-ns/my-var
  (fully-qualified? 'alias/my-var)  => actual.namespace/my-var"
  ([sym]
   (fully-qualify *ns* sym))
  ([ns sym]
    (if-let [r (ns-resolve *ns* sym)]
      (if (class? r)
        (class-name r)
        (-> r meta :ns str
            (str "/" (.sym r))
            symbol))
      sym)))

(defn fully-qualified?
  "Returns true if the symbol constitutes an absolute reference.
  Handles namespace aliases.
  ns defaults to *ns*.

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
  "Returns the unqualified version of sym.
  Handles namespace aliases.
  ns defaults to *ns*.

  (unqualifiy 'clojure.lang.IRecord) => IRecord
  (unqualifiy 'my-ns/my-var)         => my-var
  (unqualifiy 'alias/my-var)         => alias/my-var"
  ([sym]
   (unqualify *ns* sym))
  ([ns sym]
   (if (fully-qualified? sym)
     (let [[_ ns-part name] (re-matches fully-qualified-regex (str sym))
           shortcut (-> (->> ns ns-aliases
                             (map (fn [[sym ns]]
                                    [(-> ns str symbol)
                                     sym]))
                             (into {}))
                        (get (symbol ns-part)))]
       (if (and shortcut
                (not (contains? (ns-syms ns) sym)))
         (symbol (str shortcut \/ name))
         (symbol name)))
     sym)))

;; Taken and adapted from clojure.contrib
(defmacro with-ns
  "Evaluates body in another namespace. ns is either a namespace
  object or a symbol.  This makes it possible to define functions in
  namespaces other than the current one."
  [ns & body]
  `(binding [*ns* (the-ns ~ns)]
     (eval (quote (do ~@body)))))

(defmacro once-ns
  "Ensures a namespace is loaded only once, even after using 'require' or 'use'
  with :reload or :reload-all. Especially useful for namespaces that monkeypatch
  other namespaces. SHOULD immediately wrap (ns ...).

  Usage:
  (require 'shuriken.core) # or shuriken.namespace

  (shuriken.core/once-ns
    (ns my-namespace)

    (def original-func func)

    (defn func [& args]
      (swap! counter inc)
      (apply func args)))"
  [& body]
  (when-not (find-ns (-> body first second))
    (println "ONCE-NS" (find-ns (-> body first second)))
    `(eval `(do ~@'~body))))
