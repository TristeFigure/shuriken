(ns shuriken.context
  (:use clojure.pprint)
  (:require [clojure.walk :refer [macroexpand-all]]))

;; TODO
(defn ns-clear
  "Clears a namespace of its vars, keeping those from clojure.core."
  ([]
   (ns-clear *ns*))
  ([ns]
   (let [clj-ns (the-ns 'clojure.core)]
     (->> (ns-map ns)
          (filter (fn [[_sym v]]
                    (and (var? v) (-> v .ns (= clj-ns)))))
          (map (partial ns-unmap ns))
                                                          ))))

(defn clear-all-ns []
  "Strips the current namespace of its vars and deletes all other namespaces
  except clojure.core."
  (let [clj-ns (the-ns 'clojure.core)]
    (doseq [ns (all-ns)
            :when (not= ns clj-ns)]
      (if (= *ns* ns)
        (ns-clear ns)
        (remove-ns (.getName ns))))))

(def contexts
  "The global store for contexts. An atom containing a map of maps:
    {key -> {symbol -> value}}"
  (atom {}))

(defn context! [k ctx]
  "Stores a map in memory under key k. Merges with the previous context if
  present."
  (swap! contexts #(assoc % k (merge (get % k) ctx))))

(defn context [k]
  "Fetches a map stored in memory under key k."
  (get @contexts k))

(defn delete-context!
  "Deletes the map stored in memory under key k."
  [k]
  (swap! context dissoc k)
  nil)

(defn clean-params)

(defmacro lexical-map [params]
  (let [params (->> (if (and (seq? params) (-> params first (= 'quote)))
                      (second params)
                      params)
                    (map (fn [expr]
                           (if (and (seq? expr) (-> expr first (= 'quote)))
                             (second expr)
                             expr))))]
    (assert (every? symbol? params))
    (->> params
         (map (fn [sym] `['~sym ~sym]))
         (into {}))))

(defmacro letmap [m & body]
  (let [m (as-> (macroexpand-all m) $
            (if (and (seq? $) (-> $ first (= 'quote)))
              (second $)
              $)
            (map (fn [[k v]]
                   [(if (and (seq? k) (-> k first (= 'quote)))
                      (second k)
                      k)
                    v])
                 $)
            (into {} $))]
    (assert (map? m)
            (str "m should be or expand to a litteral map via "
                 "clojure.walk/macroexpand-all."))
    (assert (every? #(or (symbol? %) (keyword %))
                    (keys m)))
    (assert (every? (comp not namespace)
                    (keys m)))
    (let [m (->> m
                  (map (fn [[k v]]
                         [(if (symbol? k) `(quote ~k) k)
                          v]))
                  (into {}))
          m-sym (gensym "m")
          bindings
          (->> m
               (map (fn [[k _v]]
                      (let [sym (cond
                                  (keyword? k)
                                  (-> k name symbol)
                                  
                                  (and (seq? k) (-> k first (= 'quote)))
                                  (second k)
                                  
                                  :else k)]
                        [sym `(get ~m-sym ~k)])))
               (apply concat)
               vec)]
      `(let [~m-sym ~m]
         (let ~bindings ~@body)))))

(defmacro mcall [name & args]
  `((var ~name) '(~name ~@args) {} ~@args))
; (defmacro x [i] (inc i))
; (println "-->" (mcall x 1))

(defmacro binding-context
  "Retrieve a stored lexical context from memory and bind it using let."
  [key & body]
  (let [locals (or (keys (context key)) {})]
    `(letmap (lexical-map '~locals)
       ~@body)
    #_`(let ~(vec (mapcat (fn [l]
                          [l `(get-in @context [~key (quote ~l)])])
                        locals))
       ~@body)))

(context! :toc '{a 1 b 2})
(pprint (macroexpand-1 '(binding-context :toc a)))
; (pprint (binding-context :toc a))
