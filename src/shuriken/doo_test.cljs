(ns shuriken.doo-test
  (:require [cljs.test :as test]
            [doo.runner :refer-macros [doo-tests]]
            [shuriken.associative-test]))

(enable-console-print!)
(doo-tests 'shuriken.associative-test
           'shuriken.string-test)
