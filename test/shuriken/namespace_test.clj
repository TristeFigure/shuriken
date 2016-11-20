(ns shuriken.namespace-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer [fully-qualify]]
            [shuriken.virtual-test-namespace :refer [a-var] :as virtual])
  (:import [shuriken.virtual_test_namespace AType]))

(deftest test-fully-qualify
  (let [local-var :local]
    (binding [*ns* (find-ns 'shuriken.namespace-test)]
      (are [sym expected] (= (fully-qualify *ns* sym) expected)
           'Object              'java.lang.Object
           'fully-qualify       'shuriken.namespace/fully-qualify
           'local-var           'local-var
           'a-var               'shuriken.virtual-test-namespace/a-var
           'virtual/another-var 'shuriken.virtual-test-namespace/another-var
           'AType               'shuriken.virtual_test_namespace.AType))))
