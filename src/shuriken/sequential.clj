(ns shuriken.sequential
  "### Useful functions on sequential datastructures"
  (:require [potemkin :refer [import-vars]]))

(import-vars clojure.core/reduce1)

; Taken from https://stackoverflow.com/questions/43213573/get-in-for-lists
(defn get-nth-in
  "Like get-in but also works on lists."
  [init ks]
  (reduce
    (fn [a k]
      (if (associative? a)
        (get a k)
        (nth a k)))
    init
    ks))

(defn- assoc-nth [coll n v]
  (when-not (<= n (count coll))
    (throw (new IndexOutOfBoundsException)))
  (if (list? coll)
    (concat (take n coll)
            [v]
            (drop (inc n) coll))
    (assoc coll n v)))

(defn assoc-nth-in
  "Like assoc-in but also works on lists."
  [m [k & ks] v]
  (if ks
    (assoc-nth m k (assoc-nth-in (get m k) ks v))
    (assoc-nth m k v)))

(defn slice
  "Slice a seq using a delimiter predicate. There are two options:
  ```
  - :include-delimiter  false | :left | :right
                          whether to include the delimiter and where
  - :include-empty      true | false
                          whether to create empty seqs between
                          successive delimiters
  ```

  ```clojure
  (let [coll [1 1 0 1 0 0 1 1]]
    ;; the default
    (slice zero? coll) ;; by default, :include-delimiter false,
                                      :include-empty     false
    => ((1 1) (1) (1 1))

    (slice zero? coll :include-empty true)
    => ((1 1) (1) () (1 1))

    (slice zero? coll :include-delimiter :left)
    => ((1 1) (0 1) (0 1 1))

    (slice zero? coll :include-delimiter :right)
    => ((1 1 0) (1 0) (1 1))

    (slice zero? coll :include-delimiter :right :include-empty true)
    => ((1 1 0) (1 0) (0) (1 1))
    )
  ```"
  [delimiter? coll & {:keys [include-delimiter include-empty]
                      :or {include-delimiter false
                           include-empty     false}}]
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
  "Returns a vector of `[(filter pred coll) (remove pred coll)]`.

  ```clojure
  (let [coll [1 1 0 1 0 0 1 1 0]]
    (separate zero? coll)
    => [(1 1 1 1 1) (0 0 0 0)])
  ```"
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
