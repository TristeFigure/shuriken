(ns shuriken.monkey-patches.pprint-newlines
  (:require [clojure.pprint :refer :all]
            [shuriken.namespace :refer [with-ns]]
            [shuriken.monkey-patch :refer [require-from-dependent-namespaces]]))

(with-ns 'clojure.pprint
  (def ^:dynamic *print-pprint-string-newlines* false))

(require-from-dependent-namespaces
  '[clojure.pprint :refer [*print-pprint-string-newlines*]])

(require '[clojure.pprint :refer [*print-pprint-string-newlines*]])

(def ^:private pprint-default
  (-> simple-dispatch
      methods
      (get :default)))

(defmethod simple-dispatch String [s]
  (let [lines (clojure.string/split s #"\n")]
    (if *print-pprint-string-newlines*
      (pprint-logical-block
        :prefix "\""
        :per-line-prefix " "
        (doseq [l (butlast lines)]
          (print l)
          (pprint-newline :mandatory))
        (print (last lines))
        (print "\""))
      (pprint-default s))))
