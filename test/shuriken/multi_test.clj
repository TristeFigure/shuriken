(ns shuriken.multi-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer [call-multi]]))

(defmulti mm :type)

(defmethod mm :xx [m]
  (call-multi mm :yy m))

(defmethod mm :yy [m]
  (update m :yy inc))

(deftest test-call-multi
  (is (= {:type :xx :yy 1}
         (mm {:type :xx :yy 0}))))
