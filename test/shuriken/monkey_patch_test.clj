(ns shuriken.monkey-patch-test
  (:require [clojure.test :refer :all]
            [shuriken.monkey-patch :refer :all]
            [shuriken.namespace :refer [with-ns]])
  (:import MonkeyPatched))

;; TODO: require shuriken.core instead of shuriken.monkey-patch

(defmacro preserve-onces [& body]
  `(let [orig-onces# @shuriken.monkey-patch.no-reload-store/onces]
     ~@body
     (reset! shuriken.monkey-patch.no-reload-store/onces orig-onces#)))

(deftest test-once
  (testing "executes only once"
    (preserve-onces
      (is (= "foo\n"
             (with-out-str
               (once 'foo (println "foo"))
               (once 'shuriken.monkey-patch-test/foo (println "foo")))))))
  (testing "separates 'onces' by key"
    (preserve-onces
      (is (= "foo\nbar\n"
             (with-out-str
               (once 'foo (println "foo"))
               (once 'bar (println "bar")))))))
  (testing "when the key is not a litteral"
    (preserve-onces
      (is (= "foo\n"
             (with-out-str
               (let [k 'foo]
                 (once k (println "foo"))
                 (once (symbol (str *ns*) "foo") (println "foo")))))))))

(deftest test-refresh-once
  (testing "allows 'onces' to be replayed"
    (preserve-onces
      (is (= "foo\nfoo\n"
             (with-out-str
               (once 'foo (println "foo"))
               (refresh-once 'shuriken.monkey-patch-test/foo)
               (once 'foo (println "foo"))))))))

(defn run-java-patch-test []
  (testing "with clojure code"
    (testing "replace"
      (java-patch [MonkeyPatched "a" ["long"]]
        :replace [x] (+ 100 x))
      (is (= 101 (MonkeyPatched/a 1))))
    (testing "before"
      (java-patch [MonkeyPatched "b" ["long"]]
        :before [this x] (is (= x 1)))
      (is (= 2 (.b (MonkeyPatched.) 1))))
    (testing "after"
      (java-patch [MonkeyPatched "c" [Object]]
        :after [this result x]
        (is (= x 1))
        (is (= result 3))
        (+ 1000 result))
      (is (= 1003 (.c (MonkeyPatched.) 1)))))
  (testing "with javassist pseudo-java code"
    (java-patch ["MonkeyPatched" :d []]
      :after
      " { $_ = 100; } ")
    (is (= 100 (.d (MonkeyPatched.))))))

(deftest test-java-patch
  (run-java-patch-test)
  #_(testing "is idempotent"
    (run-java-patch-test)))

(run-tests)
