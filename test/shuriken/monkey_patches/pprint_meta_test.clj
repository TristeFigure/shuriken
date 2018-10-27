(ns shuriken.monkey-patches.pprint-meta-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [shuriken.monkey-patches.pprint-meta]))

(deftest test-pprint-meta
  (binding [clojure.pprint/*print-meta* true]
    (testing "works globally"
      (is (= (str
               "^{:meta 1}\n"
               " [^{:meta 2} a\n"
               "  ^{:meta 3} (:list)\n"
               "  ^{:meta 4} [:vector]\n"
               "  ^{:meta 5} #{:set}\n"
               "  ^{:meta 6} {^{:meta 7} key ^{:meta 8} value}]\n")
             (with-out-str
               (pprint ^{:meta 1} [' ^{:meta 2} a
                                   (with-meta (list :list) {:meta 3})
                                   ^{:meta 4} [:vector]
                                   ^{:meta 5} #{:set}
                                   ^{:meta 6} {' ^{:meta 7} key
                                               ' ^{:meta 8} value}])))))
    (testing "shorthands"
      (is (= "^Object [123]\n"
             (with-out-str (pprint ^Object [123]))))
      (is (= "^Object ^:static ^:dynamic ^{:meta :yes} [123]\n"
             (with-out-str (pprint ^Object ^:dynamic ^:static ^{:meta :yes}
                                   [123])))))))
