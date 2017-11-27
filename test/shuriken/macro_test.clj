(ns shuriken.macro-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all]))

(defmacro bb [code]
  `(+ 2 (b ~code)))

(defmacro b [code]
  `(inc ~code))

(defmacro a [code]
  `(do (b ~code)
       (bb ~code)))

(deftest test-macroexpand-some
  (is (= (macroexpand-some '#{a} '(shuriken.macro-test/a (+ 1 2)))
         '(do (shuriken.macro-test/b (+ 1 2))
              (shuriken.macro-test/bb (+ 1 2)))))
  (is (= (macroexpand-some '#{a b} '(shuriken.macro-test/a (+ 1 2)))
         '(do (clojure.core/inc (+ 1 2))
              (shuriken.macro-test/bb (+ 1 2)))))
  (is (= (macroexpand-some '#{a b bb} '(shuriken.macro-test/a (+ 1 2)))
         '(do (clojure.core/inc (+ 1 2))
              (clojure.core/+ 2 (clojure.core/inc (+ 1 2)))))))

(run-tests)
