(ns shuriken.monkey-patches.pprint-fn
  (:require [shuriken.monkey-patches.fn-source]
            [clojure.pprint :refer [simple-dispatch code-dispatch write-out
                                    pprint-logical-block pprint-newline]]))

;; TODO:
;; - use the text source rather than the list source.
;; - support #(abc %) style functions.
;; - support functions defined in namespaces.

;; This may require:
;; - adding the text source to &form's metadata
;; - reworking the #() reader macro to add necessary metadata
;; - traverse all namespaces and adapt existing functions metadata (not
;;   their var).
;; - patch source-fn so that it works with vars and functions.

(def use-method @#'clojure.pprint/use-method)
(def pprint-code-list @#'clojure.pprint/pprint-code-list)

(defn- hash-code [obj]
  (-> obj System/identityHashCode Integer/toHexString))

(defn- class-name [obj]
  (-> + class .getName (clojure.string/replace #"\$" "/")))

(defn pprint-fn [f]
  (pprint-logical-block
    :prefix "<" :suffix ">"
    (print (str "Fn@" (hash-code f) " " (class-name f) " "))
    (pprint-newline :linear)
    (pprint-code-list (-> f meta :source))))

(use-method simple-dispatch clojure.lang.Fn pprint-fn)
(use-method code-dispatch clojure.lang.Fn pprint-fn)
