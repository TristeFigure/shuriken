(ns shuriken.namespace-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all
             :reload true]
            [shuriken.virtual-test-namespace
             :refer [a-var .dot-var AProtocol]
             :as virtual])
  (:import [shuriken.virtual_test_namespace AType]))

(def expectations
  '{Object              java.lang.Object
    java.util.Formatter java.util.Formatter
    fully-qualify       shuriken.namespace/fully-qualify
    a-var               shuriken.virtual-test-namespace/a-var
    virtual/another-var shuriken.virtual-test-namespace/another-var
    AType               shuriken.virtual_test_namespace.AType
    AProtocol           shuriken.virtual-test-namespace/AProtocol
    Object/staticMeth   java.lang.Object/staticMeth
    not-found           not-found
    .toString           .toString
    .dot-var            shuriken.virtual-test-namespace/.dot-var})

(deftest test-fully-qualify
  (binding [*ns* (find-ns 'shuriken.namespace-test)]
    (doseq [[unqualified qualified] expectations]
      (is (= qualified
             (fully-qualify unqualified))))))

(deftest test-fully-qualified?
  (binding [*ns* (find-ns 'shuriken.namespace-test)]
    (doseq [[unqualified qualified] (dissoc expectations
                                            'not-found '.toString
                                            'java.util.Formatter)]
      (testing unqualified (is (false? (fully-qualified? unqualified))))
      (testing qualified   (is (true?  (fully-qualified? qualified)))))))

(deftest test-unqualify
  (binding [*ns* (find-ns 'shuriken.namespace-test)]
    (doseq [[unqualified qualified] expectations]
      (is (= unqualified
             (unqualify qualified))))))

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
