(ns shuriken.string-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all]))

(deftest test-words
  (is (= ["abc" "def" "xyz" "12'uçé"]
         (words "abc def \n xyz 12'uçé"))))

(deftest test-tabulate
  (is (= (str "- aaa\n"
              "- aaa\n"
              "- aaa") ;; terminal namespaces are dropped
         (tabulate
           (str "aaa\n"
                "aaa\n"
                "aaa\n")
           "- "))))

(deftest test-truncate
  (is (= "abc..." (truncate "abcd" 3)))
  (is (= "abc" (truncate "abc" 3)))
  (is (= "a" (truncate "a" 3))))

(deftest test-no-print
  (is (= 2
         (binding [*out* (new java.io.StringWriter)]
           (println "something")
           (inc 1)))))
