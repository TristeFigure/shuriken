(ns shuriken.predicates-composer-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer [and? or?]]))

(def calls
  (atom []))

(defn store-call! [key]
  (swap! calls conj key))

(defmacro with-fresh-calls [& body]
  `(do (reset! calls [])
       ~@body))

(defmacro defn-call [name params & body]
  `(defn ~name ~params
     (let [result# (do ~@body)]
       (store-call! ~(keyword name))
       result#)))

(defn assert-calls [keys]
  (is (= keys @calls)))

(def john
  {:name "john"
   :age 32
   :gender "male"})

(defn-call is-john? [person]
  (= "john" (:name person)))

(defn-call is-not-john? [person]
  (not= "john" (:name person)))

(defn-call is-adult? [person]
  (>= (:age person) 18))

(defn-call is-minor? [person]
  (< (:age person) 18))

(defn-call is-male? [person]
  (= "male" (:gender person)))

(defn-call is-female? [person]
  (= "female" (:gender person)))

(deftest test-and?
  (testing "when each predicate is true"
    (with-fresh-calls
      (let [composite (and? is-john? is-adult? is-male?)]
        (is (= true (composite john)))
        ;; calls  all predicates
        (assert-calls [:is-john? :is-adult? :is-male?]))))
  (testing "when one of the predicates fail"
    (with-fresh-calls
      (let [composite (and? is-john? is-minor? is-male?)]
        (is (= false (composite john)))
        ;; stops at the predicate that fails
        (assert-calls [:is-john? :is-minor?])))))

(deftest test-or?
  (testing "when each predicate is false"
    (with-fresh-calls
      (let [composite (or? is-not-john? is-minor? is-female?)]
        (is (= false (composite john)))
        ;; calls  all predicates
        (assert-calls [:is-not-john? :is-minor? :is-female?]))))
  (testing "when one of the predicates succeds"
    (with-fresh-calls
      (let [composite (or? is-not-john? is-adult? is-female?)]
        (is (= true (composite john)))
        ;; stops at the predicate that fails
        (assert-calls [:is-not-john? :is-adult?])))))
