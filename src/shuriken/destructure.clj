(ns shuriken.destructure
  "### Destructuring destructuring forms"
  (:use clojure.pprint))

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
    (sequential?  binding-form) (disentangle-sequential  binding-form)
    (map? binding-form)         (disentangle-associative binding-form)
    :else (throw (Exception. (str "Cannot disentangle " binding-form)))))

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
         :else [binding-form])))

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
  (let [{hash-mapping :mapping} (disentangle binding-form)]
    (->> hash-mapping
         (map (fn [[item key]]
                [key (restructure* item mapping)]))
         (into {}))))

(defn ^:no-doc restructure* [binding-form mapping]
  (cond
    (sequential? binding-form)  (restructure-sequential  binding-form mapping)
    (map? binding-form)         (restructure-associative binding-form mapping)
    :else (mapping binding-form)))

(defn- dequote [form]
  (if (and (sequential? form)
           (-> form first (= 'quote)))
    (second form)
    form))

(defn- requote [form]
  (if (and (sequential? form)
           (-> form first (= 'quote)))
    form
    (list 'quote form)))

;; TODO: turn into a function. Correct specs.
(defmacro restructure
  "Undoes what destructure does.
  
  ```clojure
  (restructure [x & {:keys [a b] c :cc d :d :or {d 3}}]
               {x 0 a 1 b 2 c 3})
  => [0 :a 1 :b 2 :cc 3 :d nil]
  ```"
  [binding-form mapping]
  (let [binding-form (clojure.walk/prewalk dequote binding-form)
        mapping (as-> mapping $
                  (dequote $)
                  (if (vector? $)
                    (->> $ (partition 2) (map vec) (into {}))
                    $)
                  (if (map? $)
                    (->> $
                         (map (fn [[k v]] [(requote k) v]))
                         (into {}))
                    $))]
    `(restructure* '~binding-form
                   ~mapping)))
