(ns shuriken.dance-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [shuriken.dance :refer [dance depth-dance break-dance backtrack]]))

(defrecord DanceTestRecord [a b c])

(deftest test-simple-dance
  #_(is (= {:a 2 :b 3 :c 4}
         (dance {:a 1 :b 2 :c 3}
                :pre? number?
                :post? number?
                :post  inc))))

(deftest test-complex-dance
  #_(is (= '[[+ 2 [- 3 [* 4 (/ 4 5) [dec 3]]]] {:cnt 3}]
         (dance '(+ 1 (- 2 (* 3 (/ 4 5) (dec 3))))
                depth-dance
                :walk?          #(and (seq? %) (-> % first (not= 'dec)))
                :pre?           #(and (seq? %) (-> % first (not= '/)))
                :pre            vec
                :post?          (fn [form ctx]
                                  [(and (number? form)
                                        (-> ctx :depth (< 4)))
                                   ctx])
                :post           (fn [x ctx] [(inc x) (update ctx :cnt inc)])
                :context        {:cnt 0}
                :return-context true))))

(deftest test-dance-on-record
  #_(let [result (dance (DanceTestRecord. 1 2 [3 4 5])
                      :post? number?
                      :post  inc)]
    (is (instance? DanceTestRecord result))
    (is (= result (DanceTestRecord. 2 3 [4 5 6])))))

(deftest test-break-dance
  #_(is (= [3 4 :needle [5 6]]
         (dance [1 2 [3 4 :needle [5 6]]]
                :pre? coll?
                :pre #(or (when (.contains % :needle)
                             (break-dance %))
                           %)))))

(deftest test-backtrack
  (testing "no params"
    (is (= [2 [2] [[1] [[[1]]]]]
           (dance [1 [1] [[1] [[[1]]]]]
                  depth-dance
                  :before (fn [x ctx]
                            (if (-> ctx :depth (>= 2))
                              (backtrack)
                              [x ctx]))
                  :post? number?
                  :post inc))))
  (testing "one param (form)"
    (is (= [2 [101] [101 101]]
           (dance [1 [1] [[1] [[[1]]]]]
                  depth-dance
                  :before (fn [x ctx]
                            (if (-> ctx :depth (>= 2))
                              (backtrack 100)
                              [x ctx]))
                  :post? number?
                  :post inc))))
  (testing "two params (form ctx)"
    (is (= [[2 [101] [101 101]] {:abc :xyz}]
           (dance [1 [1] [[1] [[[1]]]]]
                  depth-dance
                  :before (fn [x ctx]
                            (if (-> ctx :depth (>= 2))
                              (backtrack 100 {:abc :xyz})
                              [x ctx]))
                  :post? number?
                  :post inc
                  :return-context true))))
  (testing "anticipated backtracking"
    (pprint (dance [1 2 (backtrack [3 4])]
                   depth-dance
                   :debug true
                   :post? number?
                   :post inc
                   :return-context true))))
