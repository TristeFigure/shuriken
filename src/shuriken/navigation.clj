(ns shuriken.navigation)

(defn tree-seq-breadth
  "Like tree-seq, but in breadth-first order"
  [branch? children root]
  (let [walk (fn walk [node]
               (when (branch? node)
                 (let [cs (children node)]
                   (lazy-cat cs (mapcat walk cs)))))]
    (cons root (walk root))))
