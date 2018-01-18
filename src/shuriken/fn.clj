(ns shuriken.fn
  "### Tools around functions")

(defn- fn-class [f]
  (class (if (var? f) @f f)))

(defn- class-methods [class]
  (->> class .getDeclaredMethods
       (map #(vector (.getName %)
                     (count (.getParameterTypes %))))))

(defn- var-args? [methods]
  (some #(-> % first #{"getRequiredArity"})
        methods))

(defn- invoke-methods [methods]
  (->> methods
       (filter (comp #{"invoke"} first))
       (sort-by second)))

(defn arities
  "Returns a vector of numbers describing the arities of
  (multi-bodied):
    - anonymous functions like `#()` and `(fn [])`.
    - defined functions like `map` or `+`.
    - macros, by passing a var like `#'->`.
  
  Includes `##Inf` (`Double/POSITIVE_INFINITY`) if the function/macro
  is variadic."
  [f]
  (or (-> f meta ::arities)
      (let [methods (-> f fn-class class-methods)
            arities (as-> methods $
                      (invoke-methods $)
                      (map second $)
                      (if (var-args? methods)
                        (concat $ [##Inf])
                        $)
                      ;; if it is a macro
                      (if (and (var? f) (-> f meta :macro))
                        ;; substract implicit &form and &env arguments
                        (map #(- % 2) $) 
                        $))]
        (vec arities))))

(defn max-arity
  "Returns the maximum arity of `f` with the same semantics as
  `arities`."
  [f]
  (apply max (arities f)))

(defn min-arity
  "Returns the minimum arity of `f` with the same semantics as
  `arities`."
  [f]
  (apply min (arities f)))

(defn fake-arity
  "Returns a function `g` wrapping `f` so that `(arities g)`
  returns `arity` instead of `f`'s'original arity.
  `arity` can be a number or a sequence of numbers.
  Uses `##Inf` (`Double/POSITIVE_INFINITY`) to signify a variadic
  arity.
  
  Note that this function is orthogonal to Clojure's native arity
  checks and its only intended use is to force the return value of
  [[arities]]."
  [arity f]
  (let [arities (if (sequential? arity)
                  (vec arity)
                  [arity])]
    (with-meta
      (fn [& args]
        (apply f args))
      {::arities arities})))
