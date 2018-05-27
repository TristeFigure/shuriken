(ns shuriken.lazy)

(defn deep-doall [m]
  (doall (clojure.walk/postwalk identity m)))
