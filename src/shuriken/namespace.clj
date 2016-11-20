(ns shuriken.namespace)

(defn fully-qualify
  "Returns a fully-qualified symbol as if resolved through ns (defaults: *ns*)."
  ([sym]
   (fully-qualify *ns* sym))
  ([ns sym]
    (if-let [r (ns-resolve *ns* sym)]
      (if (class? r)
        (-> r str
            (clojure.string/replace #"(?:class|interface) " "")
            symbol)
        (-> r meta :ns str
            (str "/" (.sym r))
            symbol))
      sym)))
