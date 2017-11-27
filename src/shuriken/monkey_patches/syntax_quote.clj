(ns shuriken.monkey-patches.syntax-quote
  (:require [com.palletops.ns-reload :as nsdeps]
            [shuriken.namespace :refer [with-ns]]
            [shuriken.monkey-patch :refer [monkey-patch]]
            [clojure.tools.reader]
            [clojure.pprint])
  (:import RedefineClassAgent))

;; Step 1: substitute Clojure's native reader with that from
;; clojure.tools.reader and setup syntax-quote macro
; (with-ns 'clojure.core
;   (def read        clojure.tools.reader/read)
;   (def read-string clojure.tools.reader/read-string))

(monkey-patch substitute-clojure-read
  clojure.core/read
  [original & args]
  (apply clojure.tools.reader/read args))

(monkey-patch substitute-clojure-read-string
  clojure.core/read-string
  [original & args]
  (apply clojure.tools.reader/read-string args))

;; Step 2: Tag forms expanded with syntax-quote in clojure.tools.reader
(with-ns 'clojure.core
  (defn syntax-quoted [x]
    x))
(require '[clojure.core :refer [syntax-quoted]])

(monkey-patch change-tools-reader-read-syntax-quote
  clojure.tools.reader/read-syntax-quote
  [original & args]
  (let [result (apply original args)]
    `(syntax-quoted ~result)))

; ;; Also change the reader used by Clojure's Compiler to use
; ;; clojure.tools.reader/read
(defn read-delegate [reader eof-is-error eof-value _is-recursive opts]
  (clojure.tools.reader/read
    (merge {:eofthrow eof-is-error
            :eof eof-value}
           opts)
    reader))

(let [class-name "clojure.lang.LispReader"
      class-pool (javassist.ClassPool/getDefault)
      ct-class   (.get class-pool class-name)]
  (.stopPruning ct-class true)
  (when (.isFrozen ct-class)
    (.defrost ct-class))
  (let [method (->> (.getMethods ct-class)
                    (filter #(and (= "read" (.getName %))
                                  (= (count (.getParameterTypes %))
                                     5)))
                    first)]
    (.setBody
      method
      (format "{
                clojure.lang.IFn readFn = clojure.java.api.Clojure.var(
                  \"%s\", \"read-delegate\"
                );
                return readFn.applyTo(clojure.lang.RT.seq($args));
              }",
              (str *ns*)))
    (let [bytecode (.toBytecode ct-class)
          definition (java.lang.instrument.ClassDefinition.
                       (Class/forName class-name)
                       bytecode)]
      (RedefineClassAgent/redefineClasses
        (into-array java.lang.instrument.ClassDefinition[definition])))))

;; Step 3: Add (syntax-quoted x) -> `(x) translation into clojure.pprint
;;         and translate (unquote-splicing args) to ~@args as well.
(monkey-patch pprint-syntax-quoted-as-reader-macro
  clojure.pprint/pprint-reader-macro
  [original alis]
  (or (original alis)
      (when (and (contains? '#{clojure.core/syntax-quoted syntax-quoted}
                            (first alis))
                 (= 2 (count alis)))
        (.write ^java.io.Writer *out* "`")
        (clojure.pprint/write-out (eval (second alis)))
        true)))

(monkey-patch add-unquote-splicing-to-pprint-reader-macros
  clojure.pprint/reader-macros
  [original]
  (assoc original
    'clojure.core/unquote-splicing "~@"))

;; Reload functions using pprint-reader-macro.
(with-ns 'clojure.pprint
  (defn- pprint-list [alis]
    (if-not (pprint-reader-macro alis)
      (pprint-simple-list alis)))
  
  (defn- pprint-code-list [alis]
    (if-not (pprint-reader-macro alis) 
      (if-let [special-form (*code-table* (first alis))]
        (special-form alis)
        (pprint-simple-code-list alis))))
  
  (use-method simple-dispatch clojure.lang.ISeq pprint-list)
  (use-method code-dispatch   clojure.lang.ISeq pprint-code-list))

; ;; Step 5: reload clojure.core from dependent namespaces
(doseq [ns-sym '[clojure.core clojure.pprint]]
  (doseq [dep-ns-sym (concat (nsdeps/dependent-namespaces ns-sym)
                             (when (find-ns 'user) '[user]))]
    (eval
      `(with-ns '~dep-ns-sym
         (require ['~ns-sym :reload true])))))
