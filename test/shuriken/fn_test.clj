(ns shuriken.fn-test
  (:require [clojure.test :refer :all]
            [shuriken.fn :refer :all]))

;; TODO: require shuriken.core instead of shuriken.fn

(defmacro m ([a]) ([a b]))
(defmacro mx [])
  
(deftest test-arities
  (testing "with an anonymous #(… %1) function"
    (is (= [1]             (arities #(+ % 32))))
    (is (= [1]             (arities #(+ %1 32))))
    (is (= [2]             (arities #(+ %1 %2))))
    (is (= [13]            (arities
                             #(+ %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13))))
    (is (= [##Inf]         (arities #(apply + %&))))
    (is (= [##Inf]         (arities #(apply + % %&)))))
  (testing "with an anonymous (fn [] …) function"
    (testing "single body"
      (is (= [0]           (arities (fn []))))
      (is (= [1]           (arities (fn [a]))))
      (is (= [2]           (arities (fn [a b]))))
      (is (= [20]          (arities
                             (fn [a b c d e f g h i j k l m n o p q r s t]))))
      (is (= [##Inf]       (arities (fn [a b & more])))))
    (testing "multiple bodies"
      (is (= [0]           (arities (fn ([])))))
      (is (= [1]           (arities (fn ([a])))))
      (is (= [1 2]         (arities (fn ([a]) ([a b])))))
      (is (= [1 ##Inf]     (arities (fn ([a]) ([a b & c])))))))
  (testing "with a defined function"
    (is (= [1 2 3 4 ##Inf] (arities map)))
    (is (= [0 1 2 ##Inf]   (arities +)))
    (is (= [1]             (arities inc))))
  (testing "with a var to a macro"
    (is (= [##Inf]         (arities #'->)))
    (is (= [1 2]           (arities #'m)))
    (is (= [0]             (arities #'mx)))))

(deftest test-max-arity
  (is (= 2 (max-arity (fn ([a]) ([a b]))))))

(deftest test-min-arity
  (is (= 1 (min-arity (fn ([a]) ([a b]))))))

(deftest test-fake-arity
  (is (= [3]     (arities (fake-arity (+ 1 2) (fn [& args])))))
  (is (= [3]     (arities (fake-arity [3]     (fn [& args])))))
  (is (= [##Inf] (arities (fake-arity ##Inf   (fn [a]))))))
