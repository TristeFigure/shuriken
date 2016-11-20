(ns shuriken.core)

(defn and? [& preds]
  (fn [x]
    (loop [[p & more] preds]
      (if (p x)
        (if (seq more)
          (recur more)
          true)
        false))))

(defn or? [& preds]
  (fn [x]
    (loop [[p & more] preds]
      (if (p x)
        true
        (if (seq more)
          (recur more)
          false)))))

(def not? complement)

(defn fully-qualify
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
