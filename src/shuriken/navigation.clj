(ns shuriken.navigation
  "### More datastructure walking")

(defn tree-seq-breadth
  "Like tree-seq, but in breadth-first order"
  [branch? children root]
  (let [walk (fn walk [node]
               (when (branch? node)
                 (let [cs (children node)]
                   (lazy-cat cs (mapcat walk cs)))))]
    (cons root (walk root))))

(defn prepostwalk
  "A combination of `clojure.walk`'s `prewalk` and `postwalk`.
  Recursively modifies `form` with `pre-fn` when walking in and
  `post-fn` when walking out."
  [pre-fn post-fn form]
  (clojure.walk/walk
    (partial prepostwalk pre-fn post-fn)
    post-fn
    (pre-fn form)))

(defn prepostwalk-demo
  "Demonstrates the behavior of [[prepostwalk]]. Returns form."
  [form]
  (prepostwalk #(do (print "Walked into:   ") (prn %) %)
               #(do (print "Walked out of: ") (prn %) %)
               form))
