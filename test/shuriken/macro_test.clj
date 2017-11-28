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

(deftest test-clean-code
  (is (not= '(a b c) ``~(a b c)))
  (is (= '(a b c)
         (clean-code ``~(a b c)))))

(deftest test-file-eval
  (is (= 4
         (inc (file-eval '(+ 1 2))))))

(deftest test-macroexpand-some
  (is (= '(do (shuriken.macro-test/b (+ 1 2))
           (shuriken.macro-test/bb (+ 1 2)))
         (macroexpand-some '#{a} '(shuriken.macro-test/a (+ 1 2)))))
  (is (= '(do (clojure.core/inc (+ 1 2))
           (shuriken.macro-test/bb (+ 1 2)))
         (macroexpand-some '#{a b} '(shuriken.macro-test/a (+ 1 2)))))
  (is (= '(do (clojure.core/inc (+ 1 2))
           (clojure.core/+ 2 (clojure.core/inc (+ 1 2))))
         (macroexpand-some '#{a b bb} '(shuriken.macro-test/a (+ 1 2))))))

(deftest test-macroexpand-n
  (is (= (macroexpand-1 '(a 1))
         (macroexpand-n 1 '(a 1)))))
