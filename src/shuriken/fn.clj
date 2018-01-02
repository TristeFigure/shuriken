(ns shuriken.fn
  (:use clojure.pprint)
  (:require [clojure.spec.alpha :as s]
            [shuriken.spec :refer [conform!]]
            [shuriken.destructure :refer [restructure]]))

(defn arity
  "Returns the maximum arity of:
    - anonymous functions like `#()` and `(fn [])`.
    - defined functions like `map` or `+`.
    - macros, by passing a var like `#'->`.
  
  Returns `##Inf` (`Double/POSITIVE_INFINITY`) if the function/macro is
  variadic."
  [f]
  (let [func (if (var? f) @f f)
        methods (->> func class .getDeclaredMethods
                     (map #(vector (.getName %)
                                   (count (.getParameterTypes %)))))
        var-args? (some #(-> % first #{"getRequiredArity"})
                        methods)]
    (if var-args?
      ##Inf
      (let [max-arity (->> methods
                           (filter (comp #{"invoke"} first))
                           (sort-by second)
                           last
                           second)]
        (if (and (var? f) (-> f meta :macro))
          (- max-arity 2) ;; substract implicit &form and &env arguments
          max-arity)))))

(s/def ::arity-fn-args
       (s/cat
         :arity  any?
         :name   (s/? symbol?)
         :params vector?
         :body   (s/* any?)))

(def ^:no-doc alphabet
  (map gensym '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

(defmacro ^:no-doc arity-case [arity name params & body]
  (let [args-sym (gensym 'args)]
    `(case ~arity
       ~@(->> (concat (range 21) ;; 20 params max for fns in Clojure.
                      [##Inf])
              (mapcat
                (fn [n]
                  (let [artificial-params (if (= n ##Inf)
                                            ['& args-sym]
                                            (->> alphabet (take n) vec))]
                    [n `(fn ~@(keep identity [name]) ~artificial-params
                          (let [~params ~(if (= n ##Inf)
                                           args-sym
                                           artificial-params)]
                            ~@body))])))))))

(defmacro arity-fn 
  "Creates an anonymous function of n args named `a`, `b`, `c`, etc ...,
  binding `params` to `[a, b, c, etc ...]` before  running `body`.
  This is used to enforce a function's arity."
  [& args]
  (let [{:keys [arity name params body]} (conform! ::arity-fn-args args)]
    `(arity-case ~arity ~name ~params ~@body)))
