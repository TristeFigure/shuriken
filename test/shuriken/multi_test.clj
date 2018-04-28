(ns shuriken.multi-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer [thrown? call-method super-method]]))

(defmulti mm :type)

(defmethod mm :aa [m]
  (call-method mm :bb m))

(defmethod mm :bb [m]
  (update m :bb inc))

(deftest test-call-method
  (is (= {:type :aa :bb 1}
         (mm {:type :aa :bb 0}))))


(derive :test/dd :test/cc)
(derive :test/ee :test/aa)
(derive :test/ee :test/bb)

(defmethod mm :test/cc [m]
  123)

(defmethod mm :test/dd [m]
  (super-method mm :test/dd m))

(defmethod mm :test/ee [m]
  (super-method mm :test/ee m))

(deftest test-super-method
  (testing "normal behavior"
    (is (= 123 (mm {:type :test/dd}))))
  (testing "fail with multiple parents"
    (is (thrown? AssertionError
                 (mm {:type :test/ee})))))
