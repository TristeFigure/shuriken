(ns shuriken.waltz-test
  (:require [clojure.test :refer :all]
            [shuriken.waltz :refer [waltz]]))

(deftest test-waltz
  (is (= '[+ 2 [- 3 [* 4 (/ 5 6) (dec 3)]]]
         (waltz
           '(+ 1 (- 2 (* 3 (/ 4 5) (dec 3))))
           :walk? #(and (seq? %) (-> % first (not= 'dec)))
           :pre?  #(and (seq? %) (-> % first (not= '/)))
           :pre   #(if (seq? %) (vec %) %)
           :post? number?
           :post  inc))))
(run-tests)
