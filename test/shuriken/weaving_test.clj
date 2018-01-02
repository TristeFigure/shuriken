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
  (is (= 1 ((| 1) :ignored :ditto)))
  (is (= ##Inf (arity (| 1)))))

(deftest test-not|
  (is (false? ((not| number?) 3)))
  (testing "preserves arity"
    (is (= 1     (arity (not| (fn [a])))))
    (is (= 5     (arity (not| (fn [a b c d e])))))
    (is (= ##Inf (arity (not| (fn [& args])))))))

(deftest test-*|
  (is   (= [4 2 -3] ((*| inc dec -) 3)))
  (testing "preserves arity"
    (is (= 2        (arity (*| (fn [a b]) (fn [& args])))))))

(deftest test-<-|
  (is   (= 111 ((<-| + 1 10 100))))
  (is   (= 111 ((<-| + 1 10) 100)))
  (is   (= 111 ((<-| + 1) 10 100)))
  (is   (= 111 ((<-| +) 1 10 100)))
  (testing "preserves arity"
    (is (= 2   (arity (<-| (fn [a b c]) 1))))
    (is (= 1   (arity (<-| (fn [a b c]) 1 2))))
    (is (= 0   (arity (<-| (fn [a b c]) 1 2 3))))))

(deftest test-arity-comp
  (= 4 ((arity-comp inc (fn [a b] (+ a b)))
        1 2))
  (testing "preserves arity"
    (is (= 2     (arity (arity-comp inc (fn [a b] (+ a b))))))
    (is (= ##Inf (arity (arity-comp inc (fn [& more] 0)))))))

(deftest test-->|
  (is (= ((->| - inc inc) 4)
         ((comp inc inc -) 4)))
  (is (= ((->| inc inc -) 4)
         ((comp - inc inc) 4)))
  (testing "preserves arity"
    (is (= 2     (arity (->| (fn [a b] (+ a b)) inc))))
    (is (= ##Inf (arity (->| (fn [& args] 0) inc))))))

(deftest test-apply|
  (is (= [2 1] ((->| (fn xx [x] [x x])
                     (apply| (fn xxx [a b] [(inc a) b])))
                1)))
  (testing "has arity 1"
    (is (= 1 (arity (->| (fn xx [x] [x x])
                         (apply| (fn xxx [a b] [(inc a) b]))))))))

(deftest test-when|
  (is (= 11 ((when| number? identity inc) 10)))
  (is (= :a ((when| number? identity inc) :a)))
  (testing "preserves arity"
    (is (= 2     (arity (when| (fn [a b] true)    (constantly :abc)))))
    (is (= ##Inf (arity (when| (fn [& more] true) (constantly :abc)))))))

(deftest test-if|
  (is (= 11 ((if| number? inc str) 10)))
  (is (= ":a" ((if| number? inc str) :a)))
  (is (= :a ((if| number? inc) :a)))
  (testing "preserves arity"
    (is (= 2     (arity (if| (fn [a b] true)    (constantly :abc)))))
    (is (= ##Inf (arity (if| (fn [& more] true) (constantly :abc)))))))

(deftest test-tap|
  (with-fresh-calls
    (is (= 2 ((tap| inc do-nothing do-nothing) 1)))
    (assert-calls [:do-nothing :do-nothing]))
  (testing "preserves arity"
    (is (= 2     (arity (tap| (fn [a b])     (constantly 0)))))
    (is (= ##Inf (arity (tap| (constantly 0) (fn [& more] more)))))))

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
        (assert-calls [:is-john? :is-minor?]))))
  (testing "preserves arity"
    (is (= 2     (arity (and| (fn [a b] true)    (constantly true)))))
    (is (= ##Inf (arity (and| (fn [& more] true) (constantly true)))))))

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
        (assert-calls [:is-not-john? :is-adult?]))))
  (testing "preserves arity"
    (is (= 2     (arity (or| (fn [a b] true)    (constantly true)))))
    (is (= ##Inf (arity (or| (fn [& more] true) (constantly true)))))))

(deftest test-context|
  (is (= [124 {}]  ((->| (context| inc))             123 {})))
  (is (= [124 nil] ((->| (context| inc))             123)))
  (is (= [0   {}]  ((->| (context| (fn [& args] 0))) 123 {})))
  (is (= [0   nil] ((->| (context| (fn [& args] 0))) 123)))
  (is (= [103 nil] ((->| (context| inc)
                         (apply| (context| (fn [a ctx] [(inc a) ctx])))
                         (apply| (context| (fn [a ctx] (inc a))))
                         )
                    100)))
  (testing "has variadic arity"
    (is (= ##Inf (arity (->| (context| inc)))))))

(run-tests)
