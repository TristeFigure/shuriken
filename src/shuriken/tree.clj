(ns shuriken.tree
  "### Tree Ops")

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

(defn tree
  "Recursively builds a tree from `root` and functions
  with the following signature:
    - `(children      [node])`         : returns the direct children of `node`.
    - `(join-branches [node children])`: returns a datastructure holding `node`
                                         and its `children`.

  Example:

  ```clojure
  (defn divisors [n]
    (tree #(for [m (range 2 %)  :let [div (/ % m)]  :when (integer? div)]
             div)
          cons
          n))

  (divisors 12)
  ;; => (12 (6 (3) (2)) (4 (2)) (3) (2))
  ```"
  [children join-branches root]
  (let [cs (children root)]
    (join-branches root (map #(tree children join-branches %) cs))))

;; TODO: test & document & expose
(defn class-tree [mode-or-fn klass]
  (let [fetch-children (case mode-or-fn
                         :nested #(.getDeclaredClasses %)
                         mode-or-fn)]
    (tree (comp seq fetch-children)
          cons
          klass)))
