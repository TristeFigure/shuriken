(ns shuriken.exception
  "### Macros to deal with exceptions")

(defmacro silence
  "Returns `substitute` if `expr` raises an exception that matches
  `target`.
  If not provided, `substitute` is `nil`.
  Target can be:
    - a class
    - a sequence of classes
    - a predicate
  
  ```clojure
  (silence ArithmeticException (/ 1 0))
  => nil

  (silence [ArithmeticException]
    (do (println \"watch out !\")
        (/ 1 0)))
  ;; watch out !
  => nil

  (silence :substitute
           (fn [x]
             (isa? (class x) ArithmeticException))
           (/ 1 0))
  => :substitute
  ```"
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
  See [[silence]] for the semantics of `target`.
  
  ```clojure
  (thrown? ArithmeticException (/ 1 0))
  => true

  (thrown? #{ArithmeticException} (/ 1 1))
  => false

  (thrown? (fn [x]
             (isa? (class x) ArithmeticException))
           (throw (IllegalArgumentException. \"my-error\")))
  ;; raises:
  ;;   IllegalArgumentException my-error
  ```"
  `(= ::thrown!
      (silence ::thrown! ~target ~expr)))
