(ns shuriken.associative)

(defn- flatten-keys* [acc ks m]
  (if (and (map? m)
           (not (empty? m)))
    (reduce into
            (map (fn [[k v]]
                   (flatten-keys* acc (conj ks k) v))
                 m))
    (assoc acc ks m)))

(defn flatten-keys
  "Transforms a nested map into a map where keys are path through the
  original map and values are leaf values these paths lead to.

  (flatten-keys {:a {:b {:c :x, :d :y}}})
  => {[:a :b :c] :x
      [:a :b :d] :y}"
  [m]
  (if (empty? m)
    m
    (flatten-keys* {} [] m)))

(defn deflatten-keys
  "Builds a nested map out of a map obtained from flatten-keys"
  [m]
  (reduce (fn [acc [ks v]]
            (update-in acc ks
                       (fn [x]
                         (if x
                           (if (every? map? [x v])
                             (merge v x)
                             x)
                           v))))
          {} m))

(defn- deep-merge* [m1f & [m2 & more]]
  (if (not m2)
    m1f
    (let [m2f (flatten-keys m2)
          m1m2f (merge m1f m2f)]
      (apply deep-merge* m1m2f (or more [])))))

(defn deep-merge
  "Deep merge two or more nested maps. Already present keys gets overwritten
  like 'merge'"
  [m1 & more]
  (deflatten-keys (apply deep-merge*
                         (flatten-keys m1)
                         more)))
