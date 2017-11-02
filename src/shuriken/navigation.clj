(ns shuriken.navigation)

(defn tree-seq-breadth
  "Like tree-seq, but in breadth-first order"
  [branch? children root]
  (let [walk (fn walk [node]
               (when (branch? node)
                 (let [cs (children node)]
                   (lazy-cat cs (mapcat walk cs)))))]
    (cons root (walk root))))

(defn prepostwalk
  "A combination of clojure.walk's prewalk and postwalk. Recursively modifies
  form with pre-fn before descending and then with post-fn after going up."
  [pre-fn post-fn form]
  (clojure.walk/walk
    (partial prepostwalk pre-fn post-fn)
    post-fn
    (pre-fn form)))

(defn prepostwalk-demo
  "Demonstrates the behavior of prepostwalk by printing each form before
  descending into it and after going up. Returns form."
  [form]
  (prepostwalk #(do (print "Walked into:   ") (prn %) %)
               #(do (print "Walked out of: ") (prn %) %)
               form))
