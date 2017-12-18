(ns shuriken.macro-test
  (:require [clojure.test :refer :all :as test]
            [shuriken.macro :refer :all]))

;; TODO: require shuriken.core instead of shuriken.macro

(defmacro bb [code]
  `(+ 2 (b ~code)))

(defmacro b [code]
  `(inc ~code))

(defmacro a [code]
  `(do (b ~code)
       (bb ~code)))

(deftest test-is-form?
  (is (= true  (is-form? 'a '(a :z))))
  (is (= false (is-form? 'x '(a :z))))
  (is (= false (is-form? 'x 123)))
  (is (= false (is-form? 'a '[a :z]))))

(deftest test-wrap-form-unwrap-form
  (is (= '(a :z)
         (wrap-form 'a :z)
         (wrap-form 'a (wrap-form 'a :z))))
  (is (= :z
         (unwrap-form 'a '(a :z))
         (unwrap-form 'a (unwrap-form 'a '(a :z)))))
  (is (= :z
         (->> :z
              (wrap-form 'a)
              (unwrap-form 'a)))))

(deftest test-clean-code
  (testing "Asserting backtick standard behavior"
    (is (not= '(x (y z)) ``~(x (y z))))
    (is (not= 'Object/staticMeth `Object/staticMeth)))
  (is (= '(x (y z) java.lang.Object/staticMeth)
         (clean-code '(shuriken.macro-test/x
                       (shuriken.macro-test/y shuriken.macro-test/z)
                       java.lang.Object/staticMeth)))))

(deftest test-file-eval
  (is (= 2 (let [x 1] (file-eval '(+ 1 x))))))

(deftest test-macroexpand-some
  (is (= '(do (shuriken.macro-test/b (+ 1 2))
           (shuriken.macro-test/bb (+ 1 2)))
         (macroexpand-some '#{a} '(do (shuriken.macro-test/a (+ 1 2))))))
  (is (= '(do (clojure.core/inc (+ 1 2))
           (shuriken.macro-test/bb (+ 1 2)))
         (macroexpand-some '#{a b} '(shuriken.macro-test/a (+ 1 2)))))
  (is (= '(do (clojure.core/inc (+ 1 2))
           (clojure.core/+ 2 (clojure.core/inc (+ 1 2))))
         (macroexpand-some '#{a b bb} '(shuriken.macro-test/a (+ 1 2)))))
  (is (= '(let [x (do (shuriken.macro-test/b 1) (shuriken.macro-test/bb 1))] x)
         (macroexpand-some '#{a} '(let [x (shuriken.macro-test/a 1)] x)))))

(deftest test-macroexpand-n
  (is (= (macroexpand-1  '(a 1))
         (macroexpand-n 1 '(a 1)))))

(run-tests)
