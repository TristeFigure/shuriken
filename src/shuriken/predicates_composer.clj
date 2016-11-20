(ns shuriken.predicates-composer)

(defn and?
  "Takes predicates and return a predicate that executes them like 'and'."
  [& preds]
  (fn [x]
    (loop [[p & more] preds]
      (if (p x)
        (if (seq more)
          (recur more)
          true)
        false))))

(defn or?
  "Takes predicates and return a predicate that executes them like 'or'."
  [& preds]
  (fn [x]
    (loop [[p & more] preds]
      (if (p x)
        true
        (if (seq more)
          (recur more)
          false)))))

(def not?
  "Alias for 'complement'"
  complement)
