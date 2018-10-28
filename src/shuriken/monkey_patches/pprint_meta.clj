(ns shuriken.monkey-patches.pprint-meta
  (:require [shuriken.namespace :refer [with-ns unqualify]]
            [shuriken.sequential]
            [shuriken.meta]
            [shuriken.monkey-patch :refer [monkey-patch define-again
                                           require-from-dependent-namespaces]]
            [clojure.pprint]))

(deftype ThingWithMeta [thing])
(deftype ThingWithoutMeta [thing])

(def ^:private pprinters
  (->> (find-ns 'clojure.pprint)
       ns-interns
       vals
       (filter #(re-matches #"^[^/]+/pprint-.*$" (str %)))
       (map #(symbol (str (.getName ^clojure.lang.Namespace
                                    (.ns ^clojure.lang.Var %))
                          \/
                          (.sym ^clojure.lang.Var %))))
       (concat '[clojure.pprint/directive-table
                 clojure.pprint/cached-compile])))

(with-ns 'clojure.pprint
  (def ^:dynamic *print-meta* false)
  (def ^:dynamic *print-metas-in-metas* false))

(monkey-patch :pprint-meta clojure.pprint/pprint [original object & [writer]]
  (original (if (and (meta object) clojure.pprint/*print-meta*)
              (ThingWithMeta. object)
              object)
            (or writer *out*)))

(monkey-patch :write-out-meta clojure.pprint/write-out [original object]
  (cond
    (and (meta object) clojure.pprint/*print-meta*)
    (clojure.pprint/*print-pprint-dispatch* (ThingWithMeta. object))

    (instance? ThingWithoutMeta object)
    (clojure.pprint/*print-pprint-dispatch* (.thing object))

    :else (original object)))

(with-ns 'clojure.pprint
  (defn- pprint-meta-map [object]
    (let [m (meta object)
          [tag m] [(:tag m) (dissoc m :tag)]
          [bools others] (->> (shuriken.sequential/separate (comp true? val) m)
                              (map (partial into {})))]
      (when tag
        (.write
          ^java.io.Writer *out*
          (str (-> tag .getName symbol ;; .getName: either a Class or Symbol
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
        (binding [*print-meta* *print-metas-in-metas*]
          (write-out others))
        (.write ^java.io.Writer *out* " "))))

  (defn- pprint-thing-with-meta
    [^shuriken.monkey_patches.pprint_meta.ThingWithMeta wrapper]
    (let [thing (.thing wrapper)]
      (pprint-logical-block
        :prefix "^" :suffix ""
        (pprint-meta-map thing)
        (pprint-newline :linear)
        (write-out
          (shuriken.monkey_patches.pprint_meta.ThingWithoutMeta. thing))))))

(doseq [sym pprinters]
  (define-again sym))

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
