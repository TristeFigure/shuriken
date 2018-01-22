(ns shuriken.meta)

(defn without-meta
  "Sets meta of `x` to `nil`."
  [x]
  (with-meta x nil))

(defn merge-meta
  "Merge `m` into the meta of `x`."
  [x m]
  (with-meta x (merge (meta x) m)))
