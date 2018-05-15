(ns shuriken.multi-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer [thrown? call-method super-method
                                   augmentable-multi augment-method
                                   extendable-multi extend-method]]))

(derive :test/d :test/c)
(derive :test/g :test/f)
(derive :test/f :test/c)
(derive :test/e :test/a)
(derive :test/e :test/b)


(defmulti m1 :type)

(defmethod m1 :a [m]
  (call-method m1 :b m))

(defmethod m1 :b [m]
  (update m :b inc))

(deftest test-call-method
  (is (= {:type :a :b 1}
         (m1 {:type :a :b 0}))))

(defmulti m2 :type)

(defmethod m2 :test/c [m]
  123)

(defmethod m2 :test/d [m]
  (call-method (super-method m2 :test/d)
               m))

(defmethod m2 :test/e [m]
  (call-method (super-method m2 :test/e)
                m))

(deftest test-super-method
  (testing "normal behavior"
    (is (= 123 (m2 {:type :test/d}))))
  (testing "when the next super method is multiple parents away"
    (is (= 123 (m2 {:type :test/g}))))
  (testing "fail with multiple parents"
    (is (thrown? AssertionError
                 (m2 {:type :test/e})))))


(defmulti m3 :type)
(augmentable-multi m3 juxt)

(defmethod m3 :test/a [m]
  :m3)

(augment-method m3 :test/a [_]
  :augmented-m3)

(deftest test-augment
  (is (= [:m3 :augmented-m3] (m3 {:type :test/a}))))


(defmulti m4 :type)
(extendable-multi m4 juxt)

(defmethod m4 :test/a [m]
  :m4)

(defmethod m4 :test/c [m]
  :m4)

(extend-method m4 :test/f [m]
  :extended-m4)

(extend-method m4 :test/e [m]
  :extended-m4)

(deftest test-extend
  (is (= :m4                (m4 {:type :test/a})))
  (is (= [:m4 :extended-m4] (m4 {:type :test/f})))
  (is (= :m4                (m4 {:type :test/c}))))
