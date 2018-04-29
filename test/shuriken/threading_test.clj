(ns shuriken.threading-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [shuriken.string :refer [no-print]]
            [shuriken.core :refer [tap    tap->   tap->>
                                   if->   if->>
                                   when-> when->>
                                   and->  and->>
                                   or->   or->>
                                   pp->   pp->>
                                   <-]]))

;; TODO: require shuriken.core instead of shuriken.threading

(deftest test-tap->
  (is (= 1 (tap-> 1 inc)))
  (is (= "1 a\n"
         (with-out-str (tap-> 1 (println "a")))))
  (testing "with tap->>"
    (is (= 1 (tap->> 1 inc)))
    (is (= "a 1\n"
           (with-out-str (tap->> 1 (println "a")))))))

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
  (testing "with if->>"
    (is (= 2     (if->> 1  number? inc (str "a")))
        (= ":xa" (if->> :x number? inc (str "a")))))
  (testing "assserting a missing second case is equivalent to 'identity'"
    (testing "when the predicate succeeds"
      (is (= 2  (if-> 1  number? inc))))
    (testing "when the predicate fails"
      (is (= :x (if-> :x number? inc))))))

(deftest test-when->
  (is (= "125"  (-> 123 (when-> number? inc inc) str)))
  (is (= ":x" (-> :x  (when-> number? inc inc) str))))

(deftest test-pp->
  (is (= (-> 1 (/ 2) (/ 2) (/ 2))
         (no-print (pp-> 1 (/ 2) (/ 2) (/ 2)))))
  (testing "with pp->>"
    (is (= (->> 1 (/ 2) (/ 2) (/ 2))
           (no-print (pp->> 1 (/ 2) (/ 2) (/ 2)))))))

(deftest test-and->
  (is (true?  (and->  1 number? (> 0))))
  (is (false? (and->> 1 number? (> 0)))))

(deftest test-or->
  (is (true?  (or->  1 number? (> 0))))
  (is (true? (or->> 1 number? (> 0)))))

(deftest test-<-
  (is (= 124
         (-> :x
             (<- 123)
             inc))))

(run-tests)
