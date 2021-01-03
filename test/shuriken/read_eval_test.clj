(ns shuriken.read-eval-test
  (:require [clojure.test :refer :all]
            [shuriken.read-eval :refer :all]
            [clojure.java.io :as io])
  (:import [java.io FileReader PushbackReader]
           [clojure.lang LispReader]))

(deftest test-read-with
  (let [r #(-> (io/file "test/shuriken/read_eval_test.clj")
               FileReader.
               PushbackReader.)]
    (is (= (binding [*in* (r)] (read))
           (binding [*in* (r)] (read-with LispReader))))
    (is (= (read (r))
           (read-with LispReader (r))))
    (is (= (read {} (r))
           (read-with LispReader {} (r))))
    (is (= (read (r) true -1)
           (read-with LispReader (r) true -1)))
    (is (= (read (r) true -1 true)
           (read-with LispReader (r) true -1 true)))))
