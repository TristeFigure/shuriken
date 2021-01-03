(ns shuriken.monkey-patches.lazy-seq-tostring
  (:require [shuriken.monkey-patch :refer [java-patch
                                           require-from-dependent-namespaces]]
            [shuriken.namespace :refer [with-ns]]
            [shuriken.core :refer [macroexpand-do]]))

; (with-ns 'clojure.core
;   (def ^:dynamic *lazy-print-length* 10))

; (require '[clojure.core :refer [*lazy-print-length*]])

; ;; TODO: something is wrong with java-patch :replace
; (java-patch [clojure.lang.LazySeq "toString" []]
;   :after
;   [this result]
;   (let [s (if-not *lazy-print-length*
;             result
;             (let [xs (take *lazy-print-length* this)]
;               (if (not= (count xs)
;                         (count (take (inc *lazy-print-length*) this)))
;                 (concat xs '[...])
;                 xs)))]
;     (str "#lazy" (.seq s))))

; (require-from-dependent-namespaces
;   '[clojure.core :refer [*lazy-print-length*]])

; (println "-->" (.toString (lazy-seq [1 2 3])))
