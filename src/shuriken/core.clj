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
