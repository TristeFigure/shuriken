(ns shuriken.byte-buddy-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all]
            [shuriken.reflection :refer [interfaces]])
  (:import Mother))

(copy-class! Mother 'XMother)
(import 'XMother
        'XMother$Child
        'XMother$Crying
        'XMother$Infant)

(deftest test-copy-class!
  (let [mom (new XMother)
        c   (.giveBirth mom)]
    (is (instance? XMother$Child  c))
    (is (= [XMother$Crying]                        (interfaces XMother$Child)))
    (is (= #{Object XMother$Crying XMother$Infant} (ancestors XMother$Child)))))

(run-tests)
