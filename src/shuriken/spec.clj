(ns shuriken.spec
  (:require [clojure.spec.alpha :as s])
  (:require [clojure.pprint :refer [pprint]]))

;; TODO: document

(defn conf
  ([form f]     (conf form f identity))
  ([form f unf] (s/& form (s/conformer f unf))))

(defmacro either [& args]
  (let [cases (apply array-map args)]
    `(conf
       (s/alt ~@(->> (dissoc cases :unform)
                     (apply concat)))
       (fn [x#]
         (if (instance? clojure.lang.IObj x#)
           (vary-meta (val x#) (fnil assoc {}) ::source-either-case (key x#))
           (val x#)))

       (fn [x#]
         (let [original-case# (-> x# meta ::source-either-case)
               unform# ~(:unform cases)
               ex# (fn [incipit#]
                     (Exception.
                       (str incipit# " in 'either' statement:\n"
                            ~(with-out-str (pprint cases))
                            "unforming value\n"
                            (with-out-str (pprint x#)))))]
           (cond
             original-case#     [original-case# x#]
             (keyword? unform#) [unform# x#]
             (ifn? unform#)     [(unform# x#) x#]
             (nil? unform#)     (throw (ex# "Missing unform statement"))
             :else              (throw (ex# "Invalid unform statement"))))))))

(defn conform! [spec value]
  (let [result (s/conform spec value)]
    (when (= result ::s/invalid)
      (throw (Exception.
               (binding [*print-level* 7
                         *print-namespace-maps* false]
                 (str \newline
                      (with-out-str (->> (s/explain-data spec value)
                                         (mapv (fn [[k v]]
                                                 [(-> k name keyword) v]))
                                         (into {})
                                         pprint)))))))
    result))

(s/def ::args+body
  (s/cat :args vector?
         :body (conf (s/* any?)
                     #(apply list %) vec)))

(s/def ::args+bodies
  (either
    :arity-1 (conf ::args+body vector first)
    :arity-n (s/+ (s/and seq? ::args+body))
    :unform  (fn [x]
               (if (= (count x) 1)
                 :arity-1
                 :arity-n))))

(s/def ::defmacro-form
  (s/cat
    :op         symbol?
    :name       symbol?
    :doc-string (s/? string?)
    :attr-map   (s/? map?)
    :bodies     ::args+bodies))

(s/def ::fn-form
  (s/cat
    :op symbol?
    :name (s/? symbol?)
    :bodies ::args+bodies))

(s/def ::letfn-spec
  (s/cat
    :name symbol?
    :bodies ::args+bodies))

(s/def ::letfn-specs
  (s/* (s/and seq? ::letfn-spec)))
