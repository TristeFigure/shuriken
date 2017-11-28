(ns shuriken.threading)

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

(defmacro tap
  "Evaluates expressions in order returning the value of the first.
  Will thread the first expr into any subsequent expr starting with a threading
  macro symbol.
  
  (tap 123
       (println \"yo\")
       (some-> inc println))
  ; yo
  ; 124
  ; => 123"
  [x & body]
  (let [result-sym (gensym "result-")]
    `(let [~result-sym ~x]
       ~@(map (fn [expr]
                (if (some->> expr first str (re-matches #"^.*->>?$"))
                  `(-> ~result-sym ~expr)
                  expr))
              body)
       ~result-sym)))
