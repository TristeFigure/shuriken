(ns shuriken.sequential-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer [slice separate]]))

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
