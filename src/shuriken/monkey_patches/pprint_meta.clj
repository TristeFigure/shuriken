(ns shuriken.monkey-patches.pprint-meta
  (:require [shuriken.namespace :refer [with-ns unqualify]]
            [shuriken.sequential]
            [shuriken.meta]
            [shuriken.monkey-patch :refer [monkey-patch define-again
                                           require-from-dependent-namespaces]]
            [clojure.pprint]))

(deftype ThingWithMeta [thing])

(with-ns 'clojure.pprint
  (def ^:dynamic *print-meta* false))

(monkey-patch :pprint-meta clojure.pprint/pprint [original object & [writer]]
  (original (if (and (meta object) clojure.pprint/*print-meta*)
              (ThingWithMeta. object)
              object)
            (or writer *out*)))

(monkey-patch :write-out-meta clojure.pprint/write-out [original object]
  (if (and (meta object) clojure.pprint/*print-meta*)
    (clojure.pprint/*print-pprint-dispatch* (ThingWithMeta. object))
    (original object)))

(with-ns 'clojure.pprint
  (defn- pprint-meta-map [object]
    (let [m (meta object)
          [tag m] [(:tag m) (dissoc m :tag)]
          [bools others] (->> (shuriken.sequential/separate (comp true? val) m)
                              (map (partial into {})))]
      (when tag
        (.write
          ^java.io.Writer *out*
          (str (-> tag ((memfn ^Class getName)) symbol
                   shuriken.namespace/unqualify)
               " ")))
      (when (seq bools)
        (doseq [[k v] bools]
          (.write ^java.io.Writer *out*
                  (str (when-not (and (not tag) (= k (ffirst bools)))
                         "^")
                       k
                       " "))))
      (when (seq others)
        (when-not (and (not tag) (empty? bools))
          (.write ^java.io.Writer *out* "^"))
        (write-out others)
        (.write ^java.io.Writer *out* " "))))

  (defn- pprint-thing-with-meta
    [^shuriken.monkey_patches.pprint_meta.ThingWithMeta wrapper]
    (let [thing (.thing wrapper)]
      (pprint-logical-block
        :prefix "^" :suffix ""
        (pprint-meta-map thing)
        (pprint-newline :linear)
        (write-out (shuriken.meta/without-meta thing))))))

(define-again 'clojure.pprint/pprint-list)
(define-again 'clojure.pprint/pprint-vector)
(define-again 'clojure.pprint/pprint-map)
(define-again 'clojure.pprint/pprint-set)
(define-again 'clojure.pprint/pprint-pqueue)
(define-again 'clojure.pprint/pprint-simple-default)
(define-again 'clojure.pprint/pprint-ideref)
(define-again 'clojure.pprint/pprint-simple-default)

(with-ns 'clojure.pprint
  (use-method simple-dispatch
              shuriken.monkey_patches.pprint_meta.ThingWithMeta
              pprint-thing-with-meta)

  (use-method simple-dispatch clojure.lang.ISeq pprint-list)
  (use-method simple-dispatch clojure.lang.IPersistentVector pprint-vector)
  (use-method simple-dispatch clojure.lang.IPersistentMap pprint-map)
  (use-method simple-dispatch clojure.lang.IPersistentSet pprint-set)
  (use-method simple-dispatch clojure.lang.PersistentQueue pprint-pqueue)
  (use-method simple-dispatch clojure.lang.Var pprint-simple-default)
  (use-method simple-dispatch clojure.lang.IDeref pprint-ideref)
  (use-method simple-dispatch :default pprint-simple-default))

(require-from-dependent-namespaces
  '[clojure.pprint :reload true])
