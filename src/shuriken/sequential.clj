(ns shuriken.sequential
  "### Useful functions on sequential datastructures"
  (:require [potemkin :refer [import-vars]]
            [ubergraph.core :as u]
            [ubergraph.alg :as alg]))

(import-vars clojure.core/reduce1)



; Taken and adapted from
; https://stackoverflow.com/questions/43213573/get-in-for-lists
(defn get-nth
  "Like get but also works on lists."
  ([coll k] (get-nth coll k nil))
  ([coll k not-found]
   (if (associative? coll)
      (get coll k not-found)
      (nth coll k not-found))))

(defn get-nth-in
  "Like get-in but also works on lists."
  ([m ks] (get-nth-in m ks nil))
  ([m ks not-found]
   (reduce #(get-nth %1 %2 not-found)
           m ks)))

(defn assoc-nth [coll n v]
  "Like assoc but also works on lists."
  (if (or (nil? coll) (associative? coll))
    (assoc coll n v)
    (do (when-not (<= n (count coll))
          (throw (new IndexOutOfBoundsException)))
        (concat (take n coll)
                [v]
                (drop (inc n) coll)))))

(defn assoc-nth-in
  "Like assoc-in but also works on lists."
  [m [k & ks] v]
  (if ks
    (assoc-nth m k (assoc-nth-in (get-nth m k) ks v))
    (assoc-nth m k v)))

(defn update-nth
  "Like update but also works on lists."
  ([m k f]
   (assoc-nth m k (f (get-nth m k))))
  ([m k f x]
   (assoc-nth m k (f (get-nth m k) x)))
  ([m k f x y]
   (assoc-nth m k (f (get-nth m k) x y)))
  ([m k f x y z]
   (assoc-nth m k (f (get-nth m k) x y z)))
  ([m k f x y z & more]
   (assoc-nth m k (apply f (get-nth m k) x y z more))))

(defn update-nth-in
  "Like update-in but also works on lists."
  ([m ks f & args]
     (let [up (fn up [m ks f args]
                (let [[k & ks] ks]
                  (if ks
                    (assoc-nth m k (up (get-nth m k) ks f args))
                    (assoc-nth m k (apply f (get-nth m k) args)))))]
       (up m ks f args))))

(defn insert-at [s n x]
  (when (or (< n 0) (> n (count s)))
    (throw (new IndexOutOfBoundsException)))
  (let [[before after] (split-at n s)]
    (-> (->> (concat before [x] after)
             (into (empty s)))
        (as-> $ (if (seq? $) (reverse $) $)))))

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

(defn- parse-constraint [c]
  (if (= (count c) 2)
    (parse-constraint [(first c) :before (second c)])
    (let [[a word b] c
          [x y] (case word
                  (:< :before) [a b]
                  (:> :after)  [b a])
          x (if (= x :all) ::after-all x)
          y (if (= y :all) ::before-all y)]
      [x y])))

(defn- cycles [g]
  (->> g
       alg/scc
       (filter #(-> % count (> 1)))
       (mapv (fn [arr]
               (->> (take (-> arr count inc) (cycle arr))
                    (interpose :<)
                    vec)))))

(defn order
  "Order a sequence with a collection of constraints of the form:
  - [a b]
  - [a :before b]
  - [a :< b]
  - [b :after a]
  - [b :> a]

  Can specify constraints on `:all` elements.
  Throws a `{:type :contradictory-constraints}` ex-info if constraints
  are contradictory.

  Example:

  ```clojure
  (order [1 2 3] {2 1           3 :all})
  (order [1 2 3] [[2 1]         [3 :all]])
  (order [1 2 3] [[2 :before 1] [:all :after 3]])
  (order [1 2 3] [[2 :< 1]      [:all :> 3]])
  ;; (3 2 1)
  ```"
  [array constraints]
  (let [cycs (cycles (apply u/digraph constraints))
        _ (when-not (empty? cycs)
            (throw (ex-info "Contradictory constraints"
                            {:type :contradictory-constraints
                             :cycles cycs})))
        constraints (into {} (map parse-constraint constraints))
        constrained (clojure.set/union (set (keys constraints))
                                       (set (vals constraints)))
        free (filter (complement constrained) array)
        nodes (concat [::before-all] array [::after-all])
        g (as-> (u/digraph) $
            (apply u/add-nodes $ nodes)
            (apply u/add-edges $ (map reverse constraints))
            (apply u/add-edges $ (map reverse (partition 2 1 free))))
        ccs (filter #(> (count %) 1) (alg/connected-components g))
        g (apply u/add-edges g
                 (mapcat (fn [[f w g]]
                           (->> ccs
                                (filter #(empty?
                                           (clojure.set/intersection
                                             (set %)
                                             #{::before-all ::after-all})))
                                (map #(g [w (f %)]))))
                         [[first ::before-all reverse]
                          [last ::after-all identity]]))]
    (->> (alg/topsort g)
         reverse
         (remove #{::before-all ::after-all}))))

(defn takes
  "Split `coll` in sub-sequences of length n1 for the first, n2 for the second,
  etc... Appends the remaining items of coll as the final sub-sequence if they
  have not been consumed by the successive takes. If there are not enough items
  in `coll` to feed all the takes, return the subsequences built out of what
  could be consumed.

  ```clojure
  (takes [1 2 3] [:a :b])                ;; => ((:a) (:b))
  (takes [1 2 3] [:a :b :c])             ;; => ((:a) (:b :c))
  (takes [1 2 3] [:a :b :c :d :e :f])    ;; => ((:a) (:b :c) (:d :e :f))
  (takes [1 2 3] [:a :b :c :d :e :f :g]) ;; => ((:a) (:b :c) (:d :e :f) (:g))
  (takes [0 0 1 0 2] [:a :b :c :d :e])   ;; => (() () (:a) () (:b :c) (:d :e))
  ```"
  [[n & more] coll]
  (if more
    (concat
      [(take n coll)]
      (lazy-seq (takes more (drop n coll))))
    (keep #(and (seq %) %)
          (concat
            [(take n coll)]
            [(drop n coll)]))))
