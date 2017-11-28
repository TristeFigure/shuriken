(ns shuriken.threading-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer [tap]]))

(deftest test-tap
  (tap (with-out-str
         (tap (tap 123
                   (println "yo")
                   (-> println)
                   (println "yes"))
              (as-> $ (is (= $ 123)))))
       (as-> $ (is (= $ "yo\n123\nyes\n")))))
