(ns shuriken.sequential-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all]))

(deftest test-get-nth
  (is (= 2 (get-nth '(1 2 3) 1))))

(deftest test-get-nth-in
  (is (= :b    (get-nth-in '(1 (2 {:a :b}) 4) [1 1 :a])))
  (is (= nil   (get-nth-in '(1 (2 {:a :b}) 4) [1 1 :x])))
  (is (= :none (get-nth-in '(1 (2 {:a :b}) 4) [1 1 :x] :none))))

(deftest test-assoc-nth
  (is (= '(1 0 3)   (assoc-nth '(1 2 3) 1 0)))
  (is (= '(1 2 3 0) (assoc-nth '(1 2 3) 3 0)))
  (is (= true (thrown? IndexOutOfBoundsException
                       (assoc-nth '(1 2 3) 4 0)))))

(deftest test-assoc-nth-in
  (is (= '(1 (4) 3) (assoc-nth-in '(1 (2) 3) [1 0] 4)))
  (is (= '(1 (2 4) 3) (assoc-nth-in '(1 (2) 3) [1 1] 4)))
  (is (= true
         (thrown? IndexOutOfBoundsException
                  (assoc-nth-in '(1 (2) 3) [1 2] 4)))))

(deftest test-update-nth
  (is (= '(1 1 3) (update-nth '(1 2 3) 1 dec))))

(deftest test-update-nth-in
  (is (= '(1 (2 (300))) (update-nth-in '(1 (2 (3))) [1 1 0] * 100))))

(deftest test-slice
  (let [coll [1 0 1 0 1 0 0 0 1]]
    (testing ":include-empty"
      (testing "true"
        (is (= '((1) (1) (1) () () (1))
               (slice zero? coll :include-empty true))))
      (testing "false"
        (is (= '((1) (1) (1) (1))
               (slice zero? coll :include-empty false))))))
  (let [coll [1 1 0 1 1 0 1 1 0 0 1 1]]
    (testing ":include-delimiter"
      (testing "false"
        (is (= '((1 1) (1 1) (1 1) (1 1))
               (slice zero? coll :include-delimiter false))))
      (testing ":left"
        (is (= '((1 1) (0 1 1) (0 1 1) (0 1 1))
               (slice zero? coll :include-delimiter :left))))
      (testing ":left"
        (is (= '((1 1 0) (1 1 0) (1 1 0) (1 1))
               (slice zero? coll :include-delimiter :right)))))
    (testing "defaults"
      (is (= '((1 1) (1 1) (1 1) (1 1))
             (slice zero? coll)))))
  (testing "edge cases"
    (testing "beginning with a delimiter"
      (is (= '((1) (1))
             (slice zero? [0 1 0 1])))
      (is (= '((1) (1))
             (slice zero? [0 0 0 1 0 1]))))
    (testing "ending with a delimiter"
      (is (= '((1) (1))
             (slice zero? [1 0 1 0])))
      (is (= '((1) (1))
             (slice zero? [1 0 1 0 0 0 ]))))))

(deftest test-separate
  (let [coll [1 1 0 1 0 0 1 1 0]]
    (testing "typical case"
      (is (= [[0 0 0 0] [1 1 1 1 1]]
             (separate zero? coll))))
    (testing "edge cases"
      (is (= [[] [1 1 0 1 0 0 1 1 0]]
             (separate #{2} coll))))))

(deftest test-max-by-min-by
  (let [john {:name "john" :age 92}
        eric {:name "eric" :age 37}]
    (is (= john (apply max-by :age [john])))
    (is (= john (apply min-by :age [john])))
    (is (= john (apply max-by :age [john eric])))
    (is (= eric (apply min-by :age [john eric])))))


(deftest test-order
  (is (= [6 1 3 2 4 5]
         (order [1 2 3 4 5 6] [[6 1] [3 2]])))
  (is (= [6 1 3 2 4 5]
         (order [1 2 3 4 5 6] {6 1 3 2})))
  (is (= [6 1 3 2 4 5]
         (order [1 2 3 4 5 6] [[6 :before 1] [2 :after 3]])))
  (is (= [6 1 3 2 4 5]
         (order [1 2 3 4 5 6] [[6 :> 1] [2 :< 3]])))
  (is (= [6 1 2 4 5 3]
         (order [1 2 3 4 5 6] [[6 :all] [:all 3]])))
  (is (= [1 2 3 4 5 6]
         (order [1 2 3 4 5 6] {1 2 2 3 3 4 4 5 5 6})))
  (is (= [4 5 6 1 2 3 7]
         (order [1 2 3 4 5 6 7] {1 2 2 3 3 :all 4 5 5 6 6 1})))
  (is (= true
         (thrown? #(= (ex-data %)
                      {:type :contradictory-constraints
                       :cycles [[1 :> :all :> 6 :> 1]]})
                  (order [1 2 3 4 5 6] [[6 1] [1 :all] [:all 6]])))))

(deftest test-takes
  (is (= '((:a) (:b))
         (takes [1 2 3] [:a :b])))
  (is (= '((:a) (:b :c))
         (takes [1 2 3] [:a :b :c])))
  (is (= '((:a) (:b :c) (:d :e :f))
         (takes [1 2 3] [:a :b :c :d :e :f])))
  (is (= '((:a) (:b :c) (:d :e :f) (:g))
         (takes [1 2 3] [:a :b :c :d :e :f :g])))
  (is (= '(() () (:a) () (:b :c) (:d :e))
         (takes [0 0 1 0 2] [:a :b :c :d :e]))))
