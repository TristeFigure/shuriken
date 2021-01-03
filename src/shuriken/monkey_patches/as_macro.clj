(ns shuriken.monkey-patches.as-macro)

(comment
  ;; TROUVER UN AUTRE EXEMPLE
  ;; OK. Let's try to build a variant of `eval` that wraps the evaluated form
  ;; into a lexical context holding the `XXX` local variable.

  (defmacro xxx-eval [form]
    `(eval '(let [~'XXX :something-useful]
             ~form)))

  (xxx-eval XXX)
  ;= :something-useful


  ;; Nice. But remark that eval is a function, and needs the evaluated form
  ;; to be explicitly quoted.
  (eval '(do 123))

  ;; And since xxx-eval is a macro we can't `apply` it or pass it to higher
  ;; order functions.


  ;; Now imagine this:
  ;; - xxx-eval is a normal function that can be used as a macro
  ;; - to do so, one must tag one or more of that function's bodies with ^:macro
  ;; - exprs starting with xxx-eval will be macroexpanded using the body tagged
  ;;   with ^:macro that matches the arguments.
  ;;   - if the arguments do not match any ^:macro body, should it fallback on
  ;;     one of the untagged bodies ?
  ;;
  ;; Benefits:
  ;; - can
  )

(comment
  ;; GOALS
  (defn static-add
    ^:macro ([a b]
             `(int (+ (Math/round ~a)
                      (Math/round ~b))))
    ([a b] ^:macro (static-add (+ a b))))

  (let [result (static-add 3.14 4.51)]
    (println "result:" result)         ;; 8
    (println "class:"  (class result)) ;; Integer instead of Long
    )

  ;; Seems doable.
  ;; But what about (map static-add [1 1] [2 2])

  ;; MEANS
  (monkey-patch [clojure.lang.Compiler [macroexpand1 ^Object x]]
    :after
    []))
