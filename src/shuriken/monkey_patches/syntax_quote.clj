(require 'shuriken.namespace)

(shuriken.namespace/once-ns
  (ns shuriken.monkey-patches.syntax-quote
    (:require [com.palletops.ns-reload :as nsdeps]
              [shuriken.namespace :refer [with-ns]]
              [clojure.tools.reader]))

  ;; Step 1: substitute Clojure's native reader with that from
  ;; clojure.tools.reader and setup syntax-quote macro
  (with-ns 'clojure.core
    (def read        clojure.tools.reader/read)
    (def read-string clojure.tools.reader/read-string)
    (def ^:macro syntax-quote #'clojure.tools.reader/syntax-quote))

  ;; Step 2: enable `(x) to '(syntax-quote x)  translation, like ' and quote
  (with-ns 'clojure.tools.reader
    (def ^:private original-macros macros)
    (defn- macros [ch]
      (case ch
        \` (wrapping-reader 'syntax-quote)
        (original-macros ch))))

  ;; Step 3: exclude clojure.core/syntax-code from clojure.tools.reader
  (with-ns 'clojure.tools.reader
    (refer-clojure :exclude '[read read-line read-string char
                              default-data-readers *default-data-reader-fn*
                              *read-eval* *data-readers* *suppress-read*
                              syntax-quote]))

  ;; Step 4: Add (syntax-quote x) -> `(x) translation into clojure.pprint
  (require 'clojure.pprint)
  (alter-var-root #'clojure.pprint/reader-macros
                  assoc
                  'syntax-quote                  "`"
                  'clojure.core/syntax-quote     "`"
                  'clojure.core/unquote-splicing "~@")

  ;; Step 5: reload clojure.core from dependent namespaces
  (doseq [ns-sym (concat (nsdeps/dependent-namespaces 'clojure.core)
                         (when (find-ns 'user)
                           '[user]))]
    (in-ns ns-sym)
    (require '[clojure.core :refer [syntax-quote] :reload true]))

  (in-ns 'shuriken.monkey-patches.syntax-quote))
