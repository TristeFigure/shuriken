(ns shuriken.macro-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all]))

(deftest test-dequote
  (is (= (dequote [1 2 3])
         [1 2 3]))
  (is (= (dequote (quote (1 2 3)))
         '(1 2 3))))
(run-tests)
