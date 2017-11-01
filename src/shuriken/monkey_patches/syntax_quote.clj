(require 'shuriken.namespace)

(shuriken.namespace/once-ns
  (ns shuriken.monkey-patches.syntax-quote
    (:require [com.palletops.ns-reload :as nsdeps]
              [shuriken.namespace :refer [with-ns]]
              [clojure.tools.reader])
    (:import RedefineClassAgent))

  ;; Step 1: substitute Clojure's native reader with that from
  ;; clojure.tools.reader and setup syntax-quote macro
  (with-ns 'clojure.core
    (def read        clojure.tools.reader/read)
    (def read-string clojure.tools.reader/read-string)
    (def ^:macro syntax-quote #'clojure.tools.reader/syntax-quote))

  (defn read-delegate [reader eof-is-error eof-value _is-recursive opts]
    (clojure.tools.reader/read
      (merge {:eofthrow eof-is-error
              :eof  eof-value}
             opts)
      reader))

  ;; Also change the reader used by Clojure's Compiler to use
  ;; clojure.tools.reader
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

  ;; Step 2: enable `(x) to (syntax-quote x)  translation, like ' and quote
  (with-ns 'clojure.tools.reader
    (def ^:private original-macros macros)
    (defn- macros [ch]
      (case ch
        \` (wrapping-reader 'syntax-quote)
        (original-macros ch))))

  ;; Step 3: Add (syntax-quote x) -> `(x) translation into clojure.pprint
  ;;         and translate (unquote-splicing args) to ~@args as well.
  (require 'clojure.pprint)
  (alter-var-root #'clojure.pprint/reader-macros
                  assoc
                  'syntax-quote                  "`"
                  'clojure.core/syntax-quote     "`"
                  'clojure.core/unquote-splicing "~@")

  ;; Step 4: exclude clojure.core/syntax-code from clojure.tools.reader
  (with-ns 'clojure.tools.reader
    (refer-clojure :exclude '[read read-line read-string char
                              default-data-readers *default-data-reader-fn*
                              *read-eval* *data-readers* *suppress-read*
                              syntax-quote]))

  ;; Step 5: reload clojure.core from dependent namespaces
  (doseq [ns-sym (concat (nsdeps/dependent-namespaces 'clojure.core)
                         (when (find-ns 'user)
                           '[user]))]
    (in-ns ns-sym)
    (require '[clojure.core :refer [syntax-quote] :reload true]))

  (in-ns 'shuriken.monkey-patches.syntax-quote))
