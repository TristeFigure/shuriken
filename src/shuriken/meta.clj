(ns shuriken.meta)

(defmacro without-meta
  "Sets meta of x to nil."
  [x]
  `(with-meta ~x nil))
