(ns shuriken.dance-test
  (:require [clojure.test :refer :all]
            [shuriken.dance :refer [dance]]))

(deftest test-dance
  (is (= '[[+ 2 [- 3 [* 4 (/ 4 5) [dec 3]]]]
           {:cnt 3}]
         (dance
           '(+ 1 (- 2 (* 3 (/ 4 5) (dec 3))))
           :walk?          #(and (seq? %) (-> % first (not= 'dec)))
           :pre?           #(and (seq? %) (-> % first (not= '/)))
           :pre            vec
           :post?          (fn [form ctx]
                             (and (number? form)
                                  (-> ctx :depth (< 4))))
           :post           (fn [x ctx] [(inc x) (update ctx :cnt inc)])
           :context        {:cnt 0}
           :return-context true
           ))))

(run-tests)
