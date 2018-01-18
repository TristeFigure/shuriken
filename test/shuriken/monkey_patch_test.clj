(ns shuriken.monkey-patch-test
  (:require [clojure.test :refer :all]
            [shuriken.monkey-patch :refer :all]
            [shuriken.namespace :refer [with-ns]])
  (:import MonkeyPatched))

;; TODO: require shuriken.core instead of shuriken.monkey-patch

(defmacro preserve-onlys [& body]
  `(let [orig-onlys# @shuriken.monkey-patch.no-reload-store/onlys]
     ~@body
     (reset! shuriken.monkey-patch.no-reload-store/onlys orig-onlys#)))

(deftest test-only
  (testing "executes only once"
    (preserve-onlys
      (is (= "foo\n"
             (with-out-str
               (only 'foo (println "foo"))
               (only 'shuriken.monkey-patch-test/foo (println "foo")))))))
  (testing "separates 'onlys' by key"
    (preserve-onlys
      (is (= "foo\nbar\n"
             (with-out-str
               (only 'foo (println "foo"))
               (only 'bar (println "bar")))))))
  (testing "when the key is not a litteral"
    (preserve-onlys
      (is (= "foo\n"
             (with-out-str
               (let [k 'foo]
                 (only k (println "foo"))
                 (only (symbol (str *ns*) "foo") (println "foo")))))))))

(deftest test-refresh-only
  (testing "allows 'onlys' to be replayed"
    (preserve-onlys
      (is (= "foo\nfoo\n"
             (with-out-str
               (only 'foo (println "foo"))
               (refresh-only 'shuriken.monkey-patch-test/foo)
               (only 'foo (println "foo"))))))))

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
