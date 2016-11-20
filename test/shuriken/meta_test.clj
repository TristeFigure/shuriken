(ns shuriken.meta-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer [without-meta]]))

(deftest test-without-meta
  (is (nil? (-> [1 2 3]
                (with-meta {:x "x"})
                without-meta
                meta))))
