(ns shuriken.dance-test
  (:require [clojure.test :refer :all]
            [shuriken.dance :refer [dance]]))

(defrecord DanceTestRecord [a b c])

(deftest test-dance
  #_(is (= '[[+ 2 [- 3 [* 4 (/ 4 5) [dec 3]]]]
           {:cnt 3}]
         (dance
           '(+ 1 (- 2 (* 3 (/ 4 5) (dec 3))))
           :walk?          #(and (seq? %) (-> % first (not= 'dec)))
           :pre?           #(and (seq? %) (-> % first (not= '/)))
           :pre            vec
           :post?          (fn [form ctx]
                             [(and (number? form)
                                   (-> ctx :depth (< 4)))
                              ctx])
           :post           (fn [x ctx] [(inc x) (update ctx :cnt inc)])
           :context        {:cnt 0}
           :return-context true)))
  (is (= {:a 2 :b 3 :c 4}
         (dance {:a 1 :b 2 :c 3}
                :pre? number?
                :post? number?
                :post  inc
                :debug true)))
  #_(let [result (dance (DanceTestRecord. 1 2 [3 4 5])
                      :post? number?
                      :post  inc
                      :debug true)]
    (is (instance? DanceTestRecord result))
    (is (= result (DanceTestRecord. 2 3 [4 5 6])))))

(run-tests)
