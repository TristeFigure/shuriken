(ns shuriken.spec
  (:require [clojure.spec.alpha :as s])
  (:require [clojure.pprint :refer [pprint]]))

;; TODO: expose this namespace ?

(defn conf
  ([form f]     (s/& form (s/conformer f)))
  ([form f unf] (s/& form (s/conformer f unf))))

(defmacro either [& args]
  (let [cases (apply array-map args)]
    `(conf
       (s/alt ~@(->> (dissoc cases :unform)
                     (apply concat)))
       #(with-meta (val %)
          {::source-either-case (key %)})
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
             original-case#      [original-case# x#]
             (keyword? unform#) [unform# x#]
             (ifn? unform#)     [(unform# x#) x#]
             (nil? unform#)     (throw (ex# "Missing unform statement"))
             :else               (throw (ex# "Invalid unform statement"))))))))

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
