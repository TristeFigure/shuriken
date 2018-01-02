(ns shuriken.fn-test
  (:require [clojure.test :refer :all]
            [shuriken.fn :refer [arity arity-fn]]))

;; TODO: require shuriken.core instead of shuriken.fn

(defmacro m ([a]) ([a b]))
(defmacro mx [])
  
(deftest test-arity
  (testing "with an anonymous #(… %1) function"
    (is (= 1       (arity #(+ % 32))))
    (is (= 1       (arity #(+ %1 32))))
    (is (= 2       (arity #(+ %1 %2))))
    (is (= 13      (arity #(+ %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13))))
    (is (= ##Inf   (arity #(apply + %&))))
    (is (= ##Inf   (arity #(apply + % %&)))))
  (testing "with an anonymous (fn [] …) function"
    (testing "single body"
      (is (= 0     (arity (fn []))))
      (is (= 1     (arity (fn [a]))))
      (is (= 2     (arity (fn [a b]))))
      (is (= 20    (arity (fn [a b c d e f g h i j k l m n o p q r s t]))))
      (is (= ##Inf (arity (fn [a b & more])))))
    (testing "multiple bodies"
      (is (= 0     (arity (fn ([])))))
      (is (= 1     (arity (fn ([a])))))
      (is (= 2     (arity (fn ([a]) ([a b])))))
      (is (= ##Inf (arity (fn ([a]) ([a b & c])))))))
  (testing "with a defined function"
    (is (= ##Inf   (arity map)))
    (is (= ##Inf   (arity +)))
    (is (= 1           (arity inc))))
  (testing "with a var to a macro"
    (is (= ##Inf   (arity #'->)))
    (is (= 2           (arity #'m)))
    (is (= 0           (arity #'mx)))))

(deftest test-arity-fn
  (is (= 0       (arity (arity-fn 0 [& args] args))))
  (is (= 3       (arity (arity-fn 3 [& args] args))))
  (is (= [1 2 3] ((arity-fn 4 [a & [b & {:keys [c]}]]
                    [a b c])
                  1 2 :c 3))))

(run-tests)
