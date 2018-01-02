(ns shuriken.weaving
  (:use clojure.pprint)
  (:require [shuriken.fn :refer [arity arity-fn]]))

(def |
  "Alias for `constantly`."
  constantly)

(defn not|
  "Like `complement` but preserves arity.'"
  [f]
  (arity-fn (arity f) [& args]
    (not (apply f args))))

(defn *|
  "Like `juxt` but preserves arity."
  [& fns]
  (arity-fn
    (apply min (map arity fns))
    [& args]
    (apply (apply juxt fns)
           args)))

(defn <-|
  "Like `partial` but preserves arity."
  [f & args]
  (arity-fn
    (- (arity f) (count args))
    [& more-args]
    ((apply partial f (concat args more-args)))))

(defn arity-comp
  "Like `comp` but preserves arity."
  [& fns]
  (arity-fn
    (-> fns last arity)
    [& args]
    (apply (apply comp fns)
           args)))

(defn ->|
  "Like `comp` but composes functions from left to right. Preserves arity."
  [& fns]
  (apply arity-comp (reverse fns)))

(defn apply|
  "Transforms a function `f` accepting one argument, presumably a sequence, into
  a function that applies this argument to `f`."
  [f]
  (fn [arg]
    (apply f arg)))

(defn when|
  "Returns a function that will run the `fns` in order when `pred` succeeds.
  Preserves arity."
  [pred & fns]
  (arity-fn
    (apply min (map arity (cons pred fns)))
    [& args]
    (if (apply pred args)
      (apply (apply ->| fns) args)
      (if (> (count args) 1)
        args
        (first args)))))

(defn if|
  "Returns a function that will run `f` when `pred` succeeds or otherwise
  run `else` if it is provided. Preserves arity."
  ([pred f]
   (if| pred f #(if (= 1 (count %&))
                  (first %&)
                  %&)))
  ([pred f else]
   (arity-fn
     (apply min (map arity [pred f else]))
     [& args]
     (if (apply pred args)
       (apply f args)
       (apply else args)))))

(defn tap| [f & fns]
  "Returns a function that calls f then calls fns in order, passing each one
  the result of calling f before returning it. Preserves arity."
  (arity-fn
    (apply min (map arity (cons f fns)))
    [& args]
    (let [result (apply f args)]
      (last ((apply *| fns) result))
      result)))

(defn and|
  "Returns a function f that runs fns in order on the arguments of f in the
  style of 'and', i.e. breaking out of the chain upon false or nil.
  Its arity is the least arity in fns. Preserves arity."
  [& fns]
  (arity-fn
    (apply min (map arity fns))
    [& args] 
    (loop [[f & more] fns]
      (let [result (apply f args)]
        (if result
          (if (seq more)
            (recur more)
            result)
          result)))))

(defn or|
  "Returns a function that runs fns in order in the style of 'or', i.e.
  breaking out of the chain unless false or nil. Preserves arity."
  [& fns]
  (arity-fn
    (apply min (map arity fns))
    [& args]
    (loop [[f & more] fns]
      (let [result (apply f args)]
        (if result
          result
          (if (seq more)
            (recur more)
            result))))))

(defn- wrap-context [form ctx]
  [::context form ctx])

(defn- unwrap-context [x]
  (-> x rest vec))

(defn- context-wrapper [f]
  (let [wrap-f (fn wrap
                 ([form] (wrap form nil))
                 ([form ctx]
                  (let [result (f form ctx)]
                    (if (and (sequential? result)
                             (= (count result) 2))
                      result
                      [result ctx]))))]
    (fn [& args]
      (apply wrap-f args))))
     
(defn context| [f]
  (let [ar (arity f)
        new-f (case ar
                ##Inf f
                2     f
                1     (fn
                        ([form]     [(f form) nil])
                        ([form ctx] [(f form) ctx]))
                :else (throw
                        (Exception.
                          (str "Wrong arity " ar ". Expected 1, 2 "
                               "or variadic arguments."))))]
    (context-wrapper new-f)))
