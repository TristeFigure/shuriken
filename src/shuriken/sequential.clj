(ns shuriken.sequential
  "### Useful functions on sequential datastructures"
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [potemkin :refer [import-vars]]
            [ubergraph.core :as u]
            [ubergraph.alg :as alg]
            [weaving.core :refer :all]
            [shuriken.spec :refer [conform!]]
            [lexikon.core :refer [lexical-eval lexical-context]]))

(import-vars clojure.core/reduce1)

(defn get-nth
  "Like get but also works on lists."
  ([coll k] (get-nth coll k nil))
  ([coll k not-found]
   (if (or (associative? coll) (nil? coll))
     (get coll k not-found)
     (nth coll k not-found))))

(defn get-nth-in
  "Like get-in but also works on lists."
  ([m ks] (get-nth-in m ks nil))
  ([m ks not-found]
   (reduce #(get-nth %1 %2 not-found)
           m ks)))

(defn assoc-nth
  "Like assoc but also works on lists.
  Optionally accepts an initial `not-found` argument (defaults to `nil`)."
  ([coll n v] (assoc-nth {} coll n v))
  ([not-found coll n v]
   (let [coll (if (nil? coll) not-found coll)]
     (if (associative? coll)
       (assoc coll n v)
       (do (when-not (<= n (count coll))
             (throw (new IndexOutOfBoundsException)))
           (concat (take n coll)
                   [v]
                   (drop (inc n) coll)))))))

(defn assoc-nth-in
  "Like assoc-in but also works on lists.
  Optionally accepts an initial `not-found-f` argument, a fn of signature
    `(fn [path coll])`
  (defaults to `(constantly {})`)."
  ([coll ks v] (assoc-nth-in (constantly {}) coll ks v))
  ([not-found-f coll ks v] (assoc-nth-in [] not-found-f coll ks v))
  ([pth not-found-f coll [k & ks] v]
   (let [pth (conj pth k)]
     (if ks
       (assoc-nth (not-found-f pth coll) coll k
                  (assoc-nth-in pth not-found-f (get-nth coll k) ks v))
       (assoc-nth (not-found-f pth coll) coll k v)))))

(s/def ::update-nth-args
  (s/cat
    :not-found (s/? (or| coll? nil?))
    :m (or| coll? nil?)
    :k any?
    :f ifn?
    :args (s/* any?)))

(defn update-nth
  "Like update but also works on lists.
  Optionally accepts an initial `not-found` argument (defaults to `nil`)."
  [& args]
  (let [{:keys [not-found m k f args] :or {not-found {}}}
        (conform! ::update-nth-args args)]
    (assoc-nth not-found m k (apply f (get-nth m k) args))))

(s/def ::update-nth-in-args
  (s/cat
    :not-found-f (s/? ifn?)
    :m           (or| coll? nil?)
    :ks          sequential?
    :f           ifn?
    :args        (s/* any?)))

(defn update-nth-in
  "Like update-in but also works on lists.
  Optionally accepts an initial `not-found-f` argument, a fn of signature
    `(fn [path coll])`
  (defaults to `(constantly {})`)."
  [& args]
  (let [{:keys [not-found-f m ks f args] :or {not-found-f (constantly {})}}
        (conform! ::update-nth-in-args args)
        up (fn up [pth m ks f args]
             (let [[k & ks] ks
                   pth (conj pth k)]
               (if ks
                 (assoc-nth (not-found-f pth m) m k
                            (up pth (get-nth m k) ks f args))
                 (assoc-nth (not-found-f pth m) m k
                            (apply f (get-nth m k) args)))))]
    (up [] m ks f args)))

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
  "Returns a vector equal to `[(filter pred coll) (remove pred coll)]`
  but faster.

  ```clojure
  (let [coll [1 1 0 1 0 0 1 1 0]]
    (separate zero? coll))
    => [(1 1 1 1 1) (0 0 0 0)]
  ```"
  [pred coll]
  (reduce (fn [[l-acc r-acc] v]
            (if (pred v)
              [(conj l-acc v)  r-acc]
              [l-acc           (conj r-acc v)]))
          [[] []]
          coll))

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
  [s constraints]
  (let [ss (set s)
        spec-kws #{:all ::before-all ::after-all}
        _ (assert (= (count ss) (count s))
                  "Can't order a sequence whose elements are not distinct")
        filtered-constraints (filter (fn unnecessary? [c]
                                       (if (> (count c) 2)
                                         (unnecessary? [(first c) (nth c 2)])
                                         (let [[k v] c]
                                           (and (or (spec-kws k) (ss k))
                                                (or (spec-kws v) (ss v))))))
                                     constraints)
        cycs (cycles (apply u/digraph filtered-constraints))
        _ (when-not (empty? cycs)
            (throw (ex-info "Contradictory constraints"
                            {:type :contradictory-constraints
                             :cycles cycs})))
        parsed-constraints (into {} (map parse-constraint filtered-constraints))
        constrained (set/union (set (keys parsed-constraints))
                               (set (vals parsed-constraints)))
        free (filter (complement constrained) s)
        nodes (concat [::before-all] s [::after-all])
        g (as-> (u/digraph) $
            (apply u/add-nodes $ nodes)
            (apply u/add-edges $ (map reverse parsed-constraints))
            (apply u/add-edges $ (map reverse (partition 2 1 free))))
        ccs (filter #(> (count %) 1) (alg/connected-components g))
        g (apply u/add-edges g
                 (mapcat (fn [[f w g]]
                           (->> ccs
                                (filter #(empty?
                                           (set/intersection
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
  have not been consumed by the successive takes. Returns the specified seqs
  in order, possibly empty if there were not enough elements for all.

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


; (defn- containment-constraints [t]
;   (->> (tree-seq coll? seq t)
;        (filter coll?)
;        (mapcat (fn [subt]
;               (for [node subt]
;                 [subt node])))
;        (into {})))

; (defn- remove-ghost-constraints [ss constraints]
;   (->> (map parse-constraint constraints)
;        (filter (fn [[k v]]
;                  (and (ss k) (ss v))))
;        (into {})))

; (defn order-tree [root constraints]
;   )

; (let [tree [1 2 [[3 [4]] [5 6]]]]
;   (println "->>->" (order-tree tree {6 2})))

(defn compact
  "Shorthand for `(remove nil? xs)`."
  [xs]
  (remove nil? xs))

(defn get-some
  "Returns the first value present in `m` for keys `ks`, otherwise
  returns `nil`.
  Returns `nil` if `m` is empty or one of the keys is `nil`."
  [m & [k & ks]]
  (when (and (not (nil? k)))
    (get m k (apply get-some m ks))))

(def ^:private seq-fns        '#{map for filter remove keep compact take drop})
(def ^:private seq-suffixes-1 '{cat (apply concat)})
(def ^:private seq-suffixes-2 '{s   set
                                v   vec
                                m   (into {})
                                str (apply str)})

(defmacro def-compound-seq-fns []
  `(do ~@(for [seq-fn           seq-fns
               [suf1 expr1]     (conj seq-suffixes-1 [nil nil])
               [suf2 expr2]     (conj seq-suffixes-2 [nil nil])
               :let [seq-macro? (-> seq-fn resolve meta :macro)
                     nme        (symbol (str seq-fn suf1 suf2))
                     definer    (if seq-macro? `defmacro `defn)
                     impl       (remove nil? `(->> ~(if seq-macro?
                                                      `(~seq-fn ~'~@args)
                                                      `(apply ~seq-fn ~'args))
                                                   ~expr1
                                                   ~expr2))]
               :when (and (not (every? nil? [suf1 suf2]))
                          (not (ns-resolve 'clojure.core nme)))]
           (if seq-macro?
             `(defmacro ~nme [& ~'args]
                (->> `(->> (~'~seq-fn ~@~'args)
                           ~'~expr1
                           ~'~expr2)
                     (remove nil?)))
             `(defn ~nme [& ~'args]
                ~(->> `(->> (apply ~seq-fn ~'args)
                            ~expr1
                            ~expr2)
                      (remove nil?)))))))

(def-compound-seq-fns)

