(ns shuriken.fn-test
  (:use shuriken.debug)
  (:require [clojure.test :refer :all]
            [shuriken.fn :refer [arity]]))

;; TODO: require shuriken.core instead of shuriken.fn

(defmacro m ([a]) ([a b]))
(defmacro mx [])
  
(deftest test-arity
  (testing "with an anonymous #(… %1) function"
    (is (= 1           (arity #(+ % 32))))
    (is (= 1           (arity #(+ %1 32))))
    (is (= 2           (arity #(+ %1 %2))))
    (is (= 13          (arity #(+ %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13))))
    (is (= :variadic   (arity #(apply + %&))))
    (is (= :variadic   (arity #(apply + % %&)))))
  (testing "with an anonymous (fn [] …) function"
    (testing "single body"
      (is (= 0         (arity (fn []))))
      (is (= 1         (arity (fn [a]))))
      (is (= 2         (arity (fn [a b]))))
      (is (= 20        (arity (fn [a b c d e f g h i j k l m n o p q r s t]))))
      (is (= :variadic (arity (fn [a b & more])))))
    (testing "multiple bodies"
      (is (= 0         (arity (fn ([])))))
      (is (= 1         (arity (fn ([a])))))
      (is (= 2         (arity (fn ([a]) ([a b])))))
      (is (= :variadic (arity (fn ([a]) ([a b & c])))))))
  (testing "with a defined function"
    (is (= :variadic   (arity map)))
    (is (= :variadic   (arity +)))
    (is (= 1           (arity inc))))
  (testing "with a var to a macro"
    (is (= :variadic   (arity #'->)))
    (is (= 2           (arity #'m)))
    (is (= 0           (arity #'mx)))))

(run-tests)
