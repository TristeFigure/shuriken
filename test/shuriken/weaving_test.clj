(ns shuriken.weaving-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all]
            [shuriken.test-utils :refer :all])
  (:use shuriken.test-utils))

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

(defn-call do-nothing [x]
  x)

(deftest test-|
  (is (= 1 ((| 1) :ignored :ditto))))

(deftest test-not|
  (is (false? ((not| number?) 3))))

(deftest test-*|
  (is (= [4 2 -3] ((*| inc dec -) 3))))

(deftest test-<-|
  (is (= 111 ((<-| + 10 100) 1))))

(deftest test-->|
  (is (= ((->| - inc inc) 4)
         ((comp inc inc -) 4)))
  (is (= ((->| inc inc -) 4)
         ((comp - inc inc) 4))))

(deftest test-when|
  (is (= 11 ((when| number? identity inc) 10)))
  (is (= :a ((when| number? identity inc) :a))))

(deftest test-if|
  (is (= 11 ((if| number? inc str) 10)))
  (is (= ":a" ((if| number? inc str) :a)))
  (is (= :a ((if| number? inc) :a))))

(deftest test-tap|
  (with-fresh-calls
    (is (= 2 ((tap| inc do-nothing do-nothing) 1)))
    (assert-calls [:do-nothing :do-nothing])))

(deftest test-and|
  (testing "when each predicate is true"
    (with-fresh-calls
      (let [composite (and| is-john? is-adult? is-male?)]
        (is (= true (composite john)))
        ;; calls  all predicates
        (assert-calls [:is-john? :is-adult? :is-male?]))))
  (testing "when one of the predicates fail"
    (with-fresh-calls
      (let [composite (and| is-john? is-minor? is-male?)]
        (is (= false (composite john)))
        ;; stops at the predicate that fails
        (assert-calls [:is-john? :is-minor?])))))

(deftest test-or|
  (testing "when each predicate is false"
    (with-fresh-calls
      (let [composite (or| is-not-john? is-minor? is-female?)]
        (is (= false (composite john)))
        ;; calls  all predicates
        (assert-calls [:is-not-john? :is-minor? :is-female?]))))
  (testing "when one of the predicates succeds"
    (with-fresh-calls
      (let [composite (or| is-not-john? is-adult? is-female?)]
        (is (= true (composite john)))
        ;; stops at the predicate that fails
        (assert-calls [:is-not-john? :is-adult?])))))
