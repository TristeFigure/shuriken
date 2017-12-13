(ns shuriken.string-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all]))

(deftest test-tabulate
  (is (= (str "aaa\n"
              "aaa\n"
              "aaa\n")
         (tabulate
           (str "- aaa\n"
                "- aaa\n"
                "- aaa\n")
           "- "))))

(deftest test-tabulate
  (is (= "abc..." (tabulate "abcd" 3)))
  (is (= "abc" (tabulate "abc" 3)))
  (is (= "a" (tabulate "a" 3))))

(run-tests)
