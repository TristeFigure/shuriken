(ns shuriken.monkey-patches.syntax-quote-test
  (:require [clojure.test :refer :all]
            [shuriken.monkey-patches.syntax-quote]
            [clojure.pprint :refer [pprint]]))

(deftest test-syntax-quote
  (testing "syntax-quote macro behavior"
    (is (= (syntax-quote ~(concat [:a] [:b]))
           '(:a :b))))
  (testing "` reader macro expansion"
    (is (= `abc
           (syntax-quote abc))))
  (testing "pprint syntax-quote -> ` translation"
    (is (= (with-out-str (pprint ``is))
           "`clojure.test/is\n"))
    (is (= (with-out-str (pprint '(syntax-quote is)))
           "`is\n")))
  (testing "pprint unquote-splicing -> ~@ translation"
    (is (= (with-out-str (pprint '~@x))
           "~@x\n"))
    (is (= (with-out-str (pprint '(clojure.core/unquote-splicing x)))
           "~@x\n"))))
