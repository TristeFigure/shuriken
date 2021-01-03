(ns shuriken.reader-macros.diaeresis-unquote-test
  (:require [clojure.test :refer :all]
            [shuriken.exception :refer [thrown?]]
            [shuriken.namespace :refer [with-ns]]
            [shuriken.reader-macros.diaeresis-unquote]))

(def the-object (Object.))

(defmacro test-unquote []
  `(do ~the-object))

(defmacro test-diaeresis-unquote []
  `(do ¨the-object))

(deftest test-diaeresis-unquote
  (testing "asserting default behavior of ~"
    (is (true? (thrown? #"Can't embed object in code"
                        (with-ns 'shuriken.reader-macros.diaeresis-unquote-test
                          (test-unquote))))))
  (testing "overcoming ~ limitation "
    (is (= the-object (test-diaeresis-unquote)))))

(run-tests)
