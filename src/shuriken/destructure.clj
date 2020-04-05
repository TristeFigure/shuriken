(ns shuriken.destructure
  "### Destructuring destructuring forms"
  (:require [clojure.walk :refer [prewalk]]
            [clojure.set  :refer [difference intersection]]))

(declare disentangle)

(defn- disentangle-sequential [binding-form]
  (let [as    (->> binding-form (drop-while #(not= % :as)) second)
        more  (->> binding-form (drop-while #(not= % '&))  second)
        items (->> binding-form (remove (set (remove nil? [:as '& as more])))
                   vec)]
    (->> {:items items :as as :more more}
         (filter val)
         (into {}))))

(defn- disentangle-associative [binding-form]
  (let [as (binding-form :as)
        or (binding-form :or)
        ks (binding-form :keys)
        others  (dissoc binding-form :as :or :keys)
        items   (vec (distinct (concat ks (keys others))))
        mapping (merge (zipmap ks (map keyword ks))
                       others)]
    (->> {:items items :as as :or or :mapping mapping}
         (filter val)
         (into {}))))

(defn disentangle
  "Parses one level of destructuring.

  ```clojure
  (disentangle '[a b & [c]])
  => '{:items [a b], :more [c]}

  (disentangle '{:keys [a] b :b [c1 c2] :c :or {d 1} :as m})
  => '{:items [a b [c1 c2]],
       :as m,
       :or {d 1},
       :mapping {a :a, b :b, [c1 c2] :c}}
  ```"
  [binding-form]
  (cond
    (or (sequential?  binding-form) (nil? binding-form))
    (  disentangle-sequential  binding-form)
    (map? binding-form)
    (  disentangle-associative binding-form)
    :else (throw (Exception. (str "Cannot disentangle " binding-form)))))

(defn- entangle-sequential [m]
  (vec (concat (:items m)
               (when-let [more (:more m)]  ['& more])
               (when-let [as   (:as m)]    [:as as]))))

(defn- entangle-associative [m]
  (merge (:mapping m)
         (when-let [or (:or m)]  {:or or})
         (when-let [as (:as m)]  {:as as})))

(defn entangle
  "Undoes what [[disentangle]] does.

  ```clojure
  (entangle '{:items [a b]})
  ;; => [a b]
  (entangle '{:items [a b] :or {b 1}, :mapping {a :a b :b}})
  ;; => {a :a, b :b, :or {b 1}}
  ```"
  [m]
  (if (contains? m :mapping)
    (entangle-associative m)
    (entangle-sequential  m)))

(declare deconstruct)

(defn- deconstruct-sequential [binding-form]
  (let [{:keys [items as more]} (disentangle binding-form)]
    (concat (mapcat deconstruct items)
            (remove nil? [as])
            (if more (deconstruct more) []))))

(defn- deconstruct-associative [binding-form]
  (let [{:keys [items as]} (disentangle binding-form)]
    (concat (mapcat deconstruct items)
            (remove nil? [as]))))

(defn deconstruct
  "Returns a flat sequence of the symbols bound in the binding-form
  in depth-first order.

  ```clojure
  (deconstruct '[a & {:keys [x] y :_y :or {x 1} :as m}])
  => '[a x y m]
  ```"
  [binding-form]
  (vec (cond
         (sequential?  binding-form) (deconstruct-sequential  binding-form)
         (map? binding-form)         (deconstruct-associative binding-form)
         :else                       (when binding-form [binding-form]))))

(declare restructure*)

(defn- restructure-sequential [binding-form mapping]
  (let [{:keys [items more]} (disentangle binding-form)]
    (into (empty binding-form)
          (concat
            (mapv #(restructure* % mapping) items)
            (cond
              (symbol? more)
              (let [restr# (restructure* more mapping)]
                (if (map? restr#)
                  (reduce concat restr#)
                  restr#))

              (sequential? more)
              (restructure* more mapping)

              (map? more)
              (->> (restructure* more mapping) (reduce concat))

              :else [])))))

(defn- restructure-associative [binding-form mapping]
  ;; We map values to binding symbols and binding symbols to hash keys
  (let [{hash-mapping :mapping orr :or} (disentangle binding-form)]
    (->> hash-mapping
         (map (fn [[item key]]
                [key (restructure* item (if (map? mapping)
                                          #(get mapping % ::not-found)
                                          mapping))]))
         (remove #(-> % second (= ::not-found)))
         (into {}))))

(defn ^:no-doc restructure* [binding-form mapping]
  (cond
    (sequential? binding-form)  (restructure-sequential  binding-form mapping)
    (map? binding-form)         (restructure-associative binding-form mapping)
    :else (mapping binding-form)))

(defn restructure
  "Undoes what [[destructure]] does.

  ```clojure
  (restructure [x & {:keys [a b] c :cc d :d :or {d 3}}]
               {x 0 a 1 b 2 c 3})
  ;; => [0 :a 1 :b 2 :cc 3 :d nil]
  ```"
  [binding-form mapping]
  (restructure* binding-form
                (if (vector? mapping)
                  (->> mapping (partition 2) (map vec) (into {}))
                  mapping)))

(defn efface
  "Removes given symbols from a binding form.

  ```clojure
  (efface '[a & more :as all]                 'more 'all) ;; => '[a]
  (efface '[[a b] & more :as all]             '[a all])   ;; => '[[b] & more]
  (efface '{:keys [a] b :b :or {a 1} :as all} 'a)         ;; => '{b :b :as all}
  ```"
  [binding-form & syms]
  (let [efface? (set (flatten syms))]
    (prewalk (fn [form]
               (if (or (vector? form) (map? form))
                 (let [res (-> (disentangle form)
                               (update :items #(vec (remove efface? %))))
                       res (if (efface? (:more res))
                             (dissoc res :more)
                             res)
                       res (if (efface? (:as res))
                             (dissoc res :as)
                             res)
                       ks  (intersection efface? (-> res :mapping keys set))
                       res (if-not (empty? ks)
                             (apply update res :mapping dissoc ks)
                             res)
                       ks  (intersection efface? (-> res :or keys set))
                       res (if-not (empty? ks)
                             (apply update res :or      dissoc ks)
                             res)
                       res (if (empty? (:or res))
                             (dissoc res :or)
                             res)]
                   (entangle res))
                 form))
             binding-form)))

(defn efface-but
  "Similarly to [[efface]], removes all but the given syms from the
  binding form."
  [binding-form & syms]
  (apply efface binding-form (difference (set (deconstruct binding-form))
                                         (set (flatten syms)))))
