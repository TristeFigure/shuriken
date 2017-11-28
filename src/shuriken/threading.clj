(ns shuriken.threading)

(defmacro tap
  "Evaluates expressions in order returning the value of the first."
  [x & body]
  `(let [result# ~x]
     ~@body
     result#))

(defmacro tap->
  "Threads the expr through the forms like -> and returns the value of the
  initial expr."
  [x & body]
  `(let [result# ~x]
     (-> result# ~@body)
     result#))

(defmacro tap->>
  "Like tap-> but threads with ->>."
  [x & body]
  `(let [result# ~x]
     (->> result# ~@body)
     result#))
