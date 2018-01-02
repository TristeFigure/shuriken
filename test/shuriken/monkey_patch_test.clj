(ns shuriken.monkey-patch-test
  (:require [clojure.test :refer :all]
            [shuriken.monkey-patch :refer :all]
            [shuriken.namespace :refer [with-ns]]))

;; TODO: require shuriken.core instead of shuriken.monkey-patch

(defn clear-onlys []
  (let [ns (find-ns 'shuriken.monkey-patch.no-reload-store)]
    (intern ns 'onlys (atom #{}))))

(deftest test-only
  (testing "executes only once"
    (clear-onlys)
    (is (= "foo\n"
           (with-out-str
             (only 'foo (println "foo"))
             (only 'shuriken.monkey-patch-test/foo (println "foo"))))))
  (testing "separates 'onlys' by key"
    (clear-onlys)
    (is (= "foo\nbar\n"
           (with-out-str
             (only 'foo (println "foo"))
             (only 'bar (println "bar"))))))
  (testing "when the key is not a litteral"
    (clear-onlys)
    (is (= "foo\n"
           (with-out-str
             (let [k 'foo]
               (only k (println "foo"))
               (only 'shuriken.monkey-patch-test/foo (println "foo"))))))
    (println "ONLYS:" @shuriken.monkey-patch.no-reload-store/onlys)))

(deftest test-refresh-only
  (testing "allows 'onlys' to be replayed"
    (clear-onlys)
    (is (= "foo\nfoo\n"
           (with-out-str
             (only 'foo (println "foo"))
             (refresh-only 'shuriken.monkey-patch-test/foo)
             (only 'foo (println "foo")))))))
