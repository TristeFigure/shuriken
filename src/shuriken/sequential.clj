(ns shuriken.sequential
  (:require [potemkin :refer [import-vars]]))

(import-vars clojure.core/reduce1)

(defn slice
  "Slice a seq using a delimiter predicate. There are two options:
  - :include-delimiter  false | :left | :right
                        whether to include the delimiter and where
  - :include-empty      true | false
                        whether to create empty seqs between successive
                        delimiters"
  [delimiter? coll & {:keys [include-delimiter include-empty]
                      :or {include-delimiter false
                           include-empty false}}]
  (let [not-delimiter? (complement delimiter?)
        [before-first-delim from-first-delim] (split-with not-delimiter? coll)
        result
        (loop [xs from-first-delim
               acc []]
          (if (empty? xs)
            acc
            (let [delim (first xs)
                  [after-delim next-slice] (split-with not-delimiter?
                                                       (rest xs))]
              (recur
                next-slice
                (if (and (not include-empty)
                         (empty? after-delim))
                  acc
                  (let [current-slice (case include-delimiter
                                        (nil false) after-delim
                                        :left (cons delim after-delim)
                                        :right (concat after-delim
                                                       (take 1 next-slice)))]
                    (conj acc current-slice)))))))]
    (seq (if (empty? before-first-delim)
           result
           (cons (if (= include-delimiter :right)
                   (concat before-first-delim (take 1 from-first-delim))
                   before-first-delim)
                 result)))))

(defn separate
  "Returns a vector of [(filter pred coll) (remove pred coll)]"
  [pred coll]
  [(filter pred coll) (remove pred coll)])

(defn max-by
  "Returns the greatest of the elements by pred."
  ([f x] x)
  ([f x y] (if (pos? (compare (f x) (f y)))
             x
             y))
  ([f x y & more]
   (reduce1 (partial max-by f) (max-by f x y) more)))

(defn min-by
  "Returns the least of the elements by pred."
  ([f x] x)
  ([f x y] (if (neg? (compare (f x) (f y)))
             x
             y))
  ([f x y & more]
   (reduce1 (partial min-by f) (min-by f x y) more)))
