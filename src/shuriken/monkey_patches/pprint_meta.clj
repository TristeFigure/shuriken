(ns shuriken.monkey-patches.pprint-meta
  (:require [shuriken.monkey-patch :refer [monkey-patch
                                           require-from-dependent-namespaces]]
            [clojure.pprint]))

(monkey-patch :pprint-meta clojure.pprint/pprint [original object & [writer]]
  (let [writer (or writer *out*)]
    (when-let [m (meta object)]
      (binding [*out* writer] (print "^"))
      (original m writer))
    (original object writer)))

(require-from-dependent-namespaces
  '[clojure.pprint :reload true])
