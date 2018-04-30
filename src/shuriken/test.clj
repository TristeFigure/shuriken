(ns shuriken.test
  (:require [clojure.test :refer :all]))

(def calls
  (atom []))

(defn store-call! [v]
  (swap! calls conj v))

(defmacro with-fresh-calls [& body]
  `(do (reset! calls [])
       ~@body))

(defmacro defn-call [name params & body]
  `(defn ~name ~params
     (let [result# (do ~@body)]
       (store-call! ~(keyword name))
       result#)))

(defn assert-calls [vs]
  (is (= vs @calls)))
