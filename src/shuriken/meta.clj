(ns shuriken.meta)

(defn without-meta
  "Sets meta of `x` to `nil`."
  [x]
  (if (instance? clojure.lang.IObj x)
    (with-meta x nil)
    x))

(defn merge-meta
  "Merge `m` into the meta of `x`."
  [x m]
  (with-meta x (merge (meta x) m)))

(defn preserve-meta [old-expr new-expr]
  (with-meta new-expr (meta old-expr)))
