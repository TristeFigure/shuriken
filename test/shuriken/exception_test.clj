(ns shuriken.exception-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all]))

(defn assert-exception-thrown [f]
  (is (= ::thrown
         (try (f)
           (catch Throwable t
             ::thrown)))))

(deftest test-silence
  (testing "when an exception is thrown"
    (testing "and matches the provided target"
      (is (= nil
             (silence ArithmeticException
                      (/ 1 0))))
      (is (= nil
             (silence #{RuntimeException}
                      (/ 1 0))))
      (is (= nil
             (silence (fn [_] true)
                      (/ 1 0)))))
    (testing "and does not match the provided target"   
      (assert-exception-thrown #(silence IllegalArgumentException
                               (/ 1 0)))
      (assert-exception-thrown #(silence [IllegalArgumentException]
                               (/ 1 0)))
      (assert-exception-thrown #(silence (fn [_] false)
                               (/ 1 0)))))
  (testing "when an exception is not thrown"
    (is (= :abc
           (silence :xyz IllegalArgumentException :abc)))))

(deftest test-thrown?
  (testing "when an exception is thrown"
    (testing "and matches the provided target"
      (is (= true
             (thrown? ArithmeticException
                      (/ 1 0)))))
    (testing "and does not match the provided target"
      (is (assert-exception-thrown
            #(thrown? [IllegalArgumentException]
                      (/ 1 0))))))
  (testing "when an exception is not thrown"
    (is (= false
           (thrown? ArithmeticException
                    (/ 1 1)))))
  
  (is (= true
         (thrown? RuntimeException
                  (/ 1 0)))))
