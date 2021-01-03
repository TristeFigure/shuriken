(ns shuriken.monkey-patches.pprint-newlines-test
  (:require [clojure.test :refer :all]
            [shuriken.monkey-patches.pprint-newlines]
            [clojure.pprint :refer [pprint *print-pprint-newlines*]]))

(deftest test-pprint-newlines
  (let [data {:text "asd\nasd"}]
    (is (= "{:text \"asd\\nasd\"}\n"
           (with-out-str (pprint data))))
    (binding [*print-pprint-newlines* true]
      (is (= (str
               "{:text \"asd\n"
               "        asd\"}\n")
               (with-out-str (pprint data)))))))
