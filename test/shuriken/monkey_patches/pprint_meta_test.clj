(ns shuriken.monkey-patches.pprint-meta-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [shuriken.monkey-patches.pprint-meta]))

(deftest test-pprint-meta
  (binding [clojure.pprint/*print-meta* true]
    (testing "works globally"
      (let [coll ^{:meta 1} [' ^{:meta 2} a
                             (with-meta (list ^{:meta 4} [:list])
                               {:meta 3})
                             ^{:meta 5} [^{:meta 6} [:vector]]
                             ^{:meta 7} #{^{:meta 8} [:set]}
                             ^{:meta 9} {^{:meta 10} [' ^{:meta 11}  key]
                                         ^{:meta 12} [' ^{:meta 13} value]}
                             (doto (make-array Object 1)
                                   (aset 0 ^{:meta 14} [:array]))
                             '(ns ^{:meta 15} some.namespace
                               (:gen-class
                                 :methods [^{:meta 16} [m [] int]]))
                             '(binding [^{:meta 17} truc 123] truc)
                             '(var some-var)
                             (with-meta (into clojure.lang.PersistentQueue/EMPTY
                                              [1 2 3])
                               {:meta 18})]]
        (is (= (str
                 "^{:meta 1}\n"
                 " [^{:meta 2} a\n"
                 "  ^{:meta 3} (^{:meta 4} [:list])\n"
                 "  ^{:meta 5} [^{:meta 6} [:vector]]\n"
                 "  ^{:meta 7} #{^{:meta 8} [:set]}\n"
                 "  ^{:meta 9}\n"
                 "   {^{:meta 10} [^{:meta 11} key] ^{:meta 12} [^{:meta 13} value]}\n"
                 "  [^{:meta 14} [:array]]\n"
                 "  ^{:line 18, :column 31}\n"
                 "   (ns\n"
                 "    ^{:meta 15} some.namespace\n"
                 "    ^{:line 19, :column 32}\n"
                 "     (:gen-class :methods [^{:meta 16} [m [] int]]))\n"
                 "  ^{:line 21, :column 31} (binding [^{:meta 17} truc 123] truc)\n"
                 "  ^{:line 22, :column 31} #'some-var\n"
                 "  ^{:meta 18} <-(1 2 3)-<]\n")
               (with-out-str
                 (pprint coll))))))
    (testing "doesn't print metas in metas by default"
      (is (= "^{:meta [2]} [1]\n"
             (with-out-str (pprint ^{:meta ^{:meta [3]} [2]} [1])))))
    (testing "prints metas in metas on command"
      (is (= "^{:meta ^{:meta [3]} [2]} [1]\n"
             (binding [clojure.pprint/*print-metas-in-metas* true]
               (with-out-str (pprint ^{:meta ^{:meta [3]} [2]} [1]))))))
    (testing "shorthands"
      (is (= "^Object [123]\n"
             (with-out-str (pprint ^Object [123]))))
      (is (= "^Object ^:static ^:dynamic ^{:meta :yes} [123]\n"
             (with-out-str (pprint ^Object ^:dynamic ^:static ^{:meta :yes}
                                   [123])))))
    (testing "vars (IMeta but not IObj)"
      (is (= "^{:ns nil, :name nil} #<Var: --unnamed-->\n"
             (with-local-vars [x 1] (with-out-str (pprint x))))))))
