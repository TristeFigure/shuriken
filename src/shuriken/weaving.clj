(ns shuriken.weaving)

(def |
  "Alias for constantly."
  constantly)

(def not|
  "Alias for complement'"
  complement)

(def *|
  "Alias for juxt."
  juxt)

(def <-|
  "Alias for partial."
  partial)

(defn ->|
  "Same as comp but composes functions from left to right."
  [& fns]
  (apply comp (reverse fns)))

(defn when|
  "Returns a function that will run the fns in order when pred succeeds."
  [pred & fns]
  (fn when|composed [& args]
    (if (apply pred args)
      (apply (apply ->| fns) args)
      (if (> (count args) 1)
        args
        (first args)))))

(defn if|
  "Returns a function that will run f when pred succeeds or otherwise run else
  if it is provided."
  ([pred f]
   (if| pred f identity))
  ([pred f else]
    (fn if|composed [& args]
      (if (apply pred args)
        (apply f args)
        (apply else args)))))

(defn tap| [f & fns]
  "Returns a function that calls f then calls fns in order, passing each the
  result of calling f before returning it."
  (fn 
     [& args]
    (let [result (apply f args)]
      (last ((apply *| fns) result))
      result)))

(defn and|
  "Returns a function that runs fns in order in the style of 'and', i.e.
  breaking out of the chain upon false or nil."
  [& fns]
  (fn [& args]
    (loop [[f & more] fns]
      (let [result (apply f args)]
        (if result
          (if (seq more)
            (recur more)
            result)
          result)))))

(defn or|
  "Returns a function that runs fns in order in the style of 'or', i.e.
  breaking out of the chain unless false or nil."
  [& fns]
  (fn [& args]
    (loop [[f & more] fns]
      (let [result (apply f args)]
        (if result
          result
          (if (seq more)
            (recur more)
            result))))))
