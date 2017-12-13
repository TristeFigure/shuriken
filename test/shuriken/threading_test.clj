(ns shuriken.threading-test
  (:require [clojure.test :refer :all]
            [shuriken.threading :refer [tap if-> if->>]]))

;; TODO: require shuriken.core instead of shuriken.threading

(deftest test-tap
  (tap (with-out-str
         (tap (tap 123
                   (println "yo")
                   (-> println)
                   (println "yes"))
              (as-> $ (is (= $ 123)))))
       (as-> $ (is (= $ "yo\n123\nyes\n")))))

(deftest test-if->
  (is (= 2     (if-> 1  number? inc (str "a")))
      (= "a:x" (if-> :x number? inc (str "a"))))
  (testing "assserting a missing second case is equivalent to 'identity'"
    (testing "when the predicate succeeds"
      (is (= 2  (if-> 1  number? inc))))
    (testing "when the predicate fails"
      (is (= :x (if-> :x number? inc))))))

(run-tests)
