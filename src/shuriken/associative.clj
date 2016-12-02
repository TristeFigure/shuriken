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
  "Deep merge two or more nested maps."
  [m1 & more]
  (deflatten-keys (apply deep-merge*
                         (flatten-keys m1)
                         more)))

(defn raise-error-index-strategy [key entries]
  "The default index strategy. Asserts there is only one entry and returns it."
  (if (not= 1 (count entries))
    (throw
      (ex-info (pr-str "Can't index key " key " because of duplicate "
                       "entries " (map :name entries))
               {:type :index-by-duplicate-entries
                :entries (map :name entries)
                :key key}))
    (first entries)))

(defn index-by
  "Like group-by excepts it applies a strategy to each grouped collection.
  A strategy is a function with signature (key, entries) -> entry.
  The default strategy asserts there is only one entry and returns it."
  ([f coll]
   (index-by f raise-error-index-strategy coll))
  ([f strategy coll]
   (->> (group-by f coll)
        (map (fn [[k vs]]
               [k (strategy k vs)]))
        (into {}))))

(def unindex vals)
