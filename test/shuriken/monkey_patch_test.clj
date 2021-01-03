(ns shuriken.monkey-patch-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [shuriken.monkey-patch :refer :all :reload true]
            [shuriken.namespace :refer [with-ns]]
            [shuriken.byte-buddy :refer [copy-class!] :reload true])
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

(copy-class! MonkeyPatched 'MonkeyPatchedA)
(copy-class! MonkeyPatched 'MonkeyPatchedB)
(copy-class! MonkeyPatched 'MonkeyPatchedC)
(copy-class! MonkeyPatched 'MonkeyPatchedD)

(import 'MonkeyPatchedA
        'MonkeyPatchedB
        'MonkeyPatchedC
        'MonkeyPatchedD)

; (defn testouille [class]
;   (-> (buddy)
;       (redefine class)
;       make
;       ->javassist))

(defn run-java-patch-test []
  #_(testing "with clojure code"
    #_(testing "replace"
      (java-patch [MonkeyPatchedA "a" ["long"]]
        :replace [x] (+ 100 x))
      (is (= 101 (MonkeyPatchedA/a 1))))
    (testing "before"
      (println "--------------------------")
      (pprint (macroexpand
                '(java-patch [MonkeyPatchedB "b" ["long"]]
                  :before [this x] (is (= x 1)))))
      (is (= 2 (.b (MonkeyPatchedB.) 1))))
    #_(testing "after"
      (java-patch [MonkeyPatchedC "c" [Object]]
        :after [this result x]
        (is (= x 1))
        (is (= result 3))
        (+ 1000 result))
      (is (= 1003 (.c (MonkeyPatchedC.) 1)))))
  (testing "with javassist pseudo-java code"
    (java-patch ["MonkeyPatchedD" :d []]
      :after
      " { $_ = 100; } ")
    (is (= 100 (.d (MonkeyPatchedD.))))))

(deftest test-java-patch
  (run-java-patch-test)
  #_(testing "is idempotent"
    (run-java-patch-test)))

(run-tests)
