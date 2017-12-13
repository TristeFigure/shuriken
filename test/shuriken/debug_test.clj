(ns shuriken.debug-test
  (:require [clojure.test :refer :all]
            [shuriken.debug :refer [debug]])
  (:use shuriken.test-utils))

;; TODO: require shuriken.core instead of shuriken.debug

(defn-call do-something [x] x)
(defn-call do-nothing [x] x)

(deftest test-debug
  (with-fresh-calls
    (is (with-out-str
          (= 2 (debug (do-something 1)
                      (do-nothing 2)))))
    (assert-calls [:do-something :do-nothing])))

(debug
  '(let [a 1 b 2]
    (+ a b)
    (+ a b)
    (+ a b)
    (+ a b)
    (+ a b)
    (+ a b)
    (+ a b)
    (+ a b)
    (+ a b)
    (+ a b)))
(run-tests)
