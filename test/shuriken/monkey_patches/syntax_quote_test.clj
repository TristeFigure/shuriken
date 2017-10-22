(ns shuriken.monkey-patches.syntax-quote-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all]
            [clojure.pprint :refer [pprint]]))

(deftest test-syntax-quote
  (testing "syntax-quote macro behavior"
    (is (= (syntax-quote ~(concat [:a] [:b]))
           '(:a :b))))
  (testing "` reader macro expansion"
    (is (= `abc
           (syntax-quote abc))))
  (testing "pprint syntax-quote -> ` translation"
    (= (with-out-str (pprint '`abc))
       "`abc")
    (= (with-out-str (pprint '(syntax-quote abc)))
       "`abc")))
