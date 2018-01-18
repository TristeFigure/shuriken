(ns shuriken.weaving
  (:use clojure.pprint)
  (:require [shuriken.fn :refer [arities fake-arity]]))

(defn |
  "Returns a function that behvaves like `constantly` but has 1 for
  arity."
  [v]
  (fake-arity 1 (constantly v)))

(defn not|
  "Returns a function that behaves like `complement` but preserves
  arity.'"
  [f]
  (fake-arity (arities f) (complement f)))

(defn *|
  "Returns a function that behaves like `juxt` but preserves arity."
  [& fns]
  (fake-arity (->> fns (mapcat arities) distinct sort)
              #(apply (apply juxt fns) %&)))

(defn <-|
  "Returns a function that behaves like `partial` but preserves arity."
  [f & args]
  (fake-arity (map #(- % (count args))
                   (arities f))
              #((apply partial f (concat args %&)))))

(defn arity-comp
  "Returns a function that behaves like `comp` but preserves arity."
  [& fns]
  (fake-arity (-> fns last arities)
              #(apply (apply comp fns)
                      %&)))

(defn ->|
  "Returns a function that behaves like `comp` but composes functions from left to right.
  Preserves arity."
  [& fns]
  (apply arity-comp (reverse fns)))

(println)

(defn apply|
  "Transforms a function `f` accepting one argument, presumably a
  sequence, into a function that applies this argument to `f`."
  [f]
  #(apply f %1))

(defn when|
  "Returns a function that will run the `fns` in order when `pred`
  succeeds. Preserves arity."
  [pred & fns]
  (fake-arity
    (->> (cons pred fns) (mapcat arities) distinct sort)
    #(if (apply pred %&)
       (apply (apply ->| fns) %&)
       (if (= (count %&) 1)
         (first %&)
         %&))))

(defn if|
  "Returns a function that will run `f` when `pred` succeeds or
  otherwise run `else` if it is provided (defaults to `nil`).
  Preserves arity."
  ([pred f]
   (if| pred f #(if (= 1 (count %&))
                  (first %&)
                  %&)))
  ([pred f else]
   (fake-arity
     (->> [pred f else] (mapcat arities) distinct sort)
     #(if (apply pred %&)
        (apply f %&)
        (apply else %&)))))

(defn tap| 
  "Returns a function that calls `f` then calls `fns` in order,
  passing to each one the result of calling `f`, and then returns it.
  Preserves arity."
  [f & fns]
  (fake-arity
    (->> (cons f fns) (mapcat arities) distinct sort)
    #(let [result (apply f %&)]
       (last ((apply *| fns) result))
       result)))

(defn and|
  "Returns a function `f` that runs `fns` in order on the arguments of
  `f` in the style of `and`, i.e. breaking out of the chain upon
  `false` or `nil`.
  Preserves arity."
  [& fns]
  (fake-arity
    (->> fns (mapcat arities) distinct sort)
    #(loop [[f & more] fns]
       (let [result (apply f %&)]
         (if result
           (if (seq more)
             (recur more)
             result)
           result)))))

(defn or|
  "Returns a function that runs `fns` in order in the style of `or`,
  i.e. breaking out of the chain unless `false` or `nil`.
  Preserves arity."
  [& fns]
  (fake-arity
    (->> fns (mapcat arities) distinct sort)
    #(loop [[f & more] fns]
       (let [result (apply f %&)]
         (if result
           result
           (if (seq more)
             (recur more)
             result))))))

(defn- wrap-context [[form ctx]]
  [::context form ctx])

(defn- unwrap-context [x]
  (-> x rest vec))

(defn- wrapped-context? [x]
  (and (vector? x)
       (-> x first (= ::context))))

(defn- context-wrapper [f]
  (let [wrap-f (fn wrap
                 ([form]     (wrap form nil))
                 ([form ctx] (let [result (f form ctx)]
                               (if (wrapped-context? result)
                                 (unwrap-context result)
                                 result))))]
    (fake-arity (arities f)
      #(apply wrap-f %&))))

;; TODO: document
(defn context| [f]
  (let [ar (set (arities f))
        mono-ar (or (contains? ar 1)
                    (contains? ar ##Inf))
        bi-ar (contains? ar 2)
        new-f (case [mono-ar bi-ar]
                [true true] f
                [true false]  (fn
                                ([x]     (wrap-context [(f x) nil]))
                                ([x ctx] (wrap-context [(f x) ctx])))
                [false true]  (fn
                                ([x]     (wrap-context [(f x) nil]))
                                ([x ctx] (f x ctx)))
                [false false] (fn
                                ([x]     (wrap-context [(f x) nil]))
                                ([x ctx] (wrap-context [(f x) ctx]))))]
    (context-wrapper new-f)))
