(ns shuriken.flow)

(defmacro silence
  "Returns `substitute` if `expr` raises an exception that matches `target`.
  • if not provided, `substitute` is valued to nil.
  • target can be either one of :
      - a class
      - a sequence of classes
      - a predicate"
  ([target expr]
   `(silence nil ~target ~expr))
  ([substitute target expr]
   `(let [target# ~target
          pred# (cond
                  (class? target#) #(isa? (class %) target#)
                  (coll? target#) #(some (partial isa? (class %)) target#)
                  (ifn? target#)  target#
                  :else (throw (IllegalArgumentException.
                                 "target must be sequence of exception class
                                 or a function")))]
      (try
        ~expr
        (catch Throwable t#
          (if (pred# t#)
            ~substitute
            (throw t#)))))))

(defmacro thrown? [target expr]
  "Returns true if `expr` raises an exception matching `target`.
  `target` has the same semantics as with the silence fn."
  `(= ::thrown!
      (silence ::thrown! ~target ~expr)))
