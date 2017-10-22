(ns shuriken.namespace-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all
             :reload true]
            [shuriken.virtual-test-namespace
             :refer [a-var AProtocol]
             :as virtual])
  (:import [shuriken.virtual_test_namespace AType]))

(def expectations
  '{Object              java.lang.Object
    fully-qualify       shuriken.namespace/fully-qualify
    a-var               shuriken.virtual-test-namespace/a-var
    not-found           not-found
    virtual/another-var shuriken.virtual-test-namespace/another-var
    AType               shuriken.virtual_test_namespace.AType
    AProtocol           shuriken.virtual-test-namespace/AProtocol})

(deftest test-fully-qualify
  (binding [*ns* (find-ns 'shuriken.namespace-test)]
    (doseq [[unqualified qualified] expectations]
      (is (= (fully-qualify unqualified)
             qualified)))))

(deftest test-fully-qualified?
  (binding [*ns* (find-ns 'shuriken.namespace-test)]
    (doseq [[unqualified qualified] (dissoc expectations 'not-found)]
      (is (false? (fully-qualified? unqualified)))
      (is (true?  (fully-qualified? qualified))))))

(deftest test-unqualify
  (binding [*ns* (find-ns 'shuriken.namespace-test)]
    (doseq [[unqualified qualified] expectations]
      (is (= (unqualify qualified)
             unqualified)))))

(deftest fully-qualify-round-trips
  (testing "fully-qualify --> fully-qualify"
    (is (= 'java.lang.Object
           (fully-qualify (fully-qualify 'Object)))))
  (testing "unqualify --> unqualify"
    (is (= 'Object
           (unqualify (unqualify 'java.lang.Object)))))
  (testing "fully-qualify --> unqualify --> fully-qualify"
    (is (= 'java.lang.Object
           (-> 'Object fully-qualify unqualify fully-qualify))))
  (testing "unqualify --> fully-qualify --> unqualify"
    (is (= 'Object
           (-> 'java.lang.Object unqualify fully-qualify unqualify)))))

(deftest test-with-ns
  (let [initial-ns *ns*]
    (is (with-ns 'shuriken.virtual-test-namespace
          (= *ns* (the-ns 'shuriken.virtual-test-namespace))))
    (is (= *ns* initial-ns))))

(deftest test-once-ns
  (when (find-ns 'shuriken.once-test-namespace)
      (clojure.lang.Namespace/remove 'shuriken.once-test-namespace)
      (dosync (commute @#'clojure.core/*loaded-libs*
                       disj 'shuriken.once-test-namespace)))
  (testing "namespace should not be loaded yet"
    (is (nil? (find-ns 'shuriken.once-test-namespace))))
  (testing "requiring for the first time"
    (is (= (with-out-str (require 'shuriken.once-test-namespace))
           "loaded\n")))
  (testing "requiring again with :reload"
    (is (= (with-out-str (require 'shuriken.once-test-namespace :reload))
           "")))
  (testing "requiring again with :reload-all"
    (is (= (with-out-str (require 'shuriken.once-test-namespace :reload-all))
           ""))))
