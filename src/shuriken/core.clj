(ns shuriken.core
  (:require [potemkin :refer [import-vars]]
            [shuriken associative
                      byte-buddy
                      debug
                      destructure
                      exception
                      lazy
                      macro
                      meta
                      monkey-patch
                      namespace
                      tree
                      reflection
                      sequential
                      spec
                      string]))

(import-vars
  [shuriken.associative
   map-keys map-vals
   filter-keys filter-vals
   remove-keys remove-vals
   flatten-keys deflatten-keys deep-merge
   index-by unindex
   merge-with-plan
   continue|
   split-map
   map-difference
   map-intersection
   submap?
   getsoc]

  [shuriken.byte-buddy
   copy-class!]

  [shuriken.debug
   debug
   debug-print]

  [shuriken.destructure
   disentangle
   entangle
   deconstruct
   restructure]

  [shuriken.exception
   silence
   thrown?]

  [shuriken.lazy
   deep-doall]

  ;; TODO: some vars are missing
  [shuriken.macro
   is-form?
   wrap-form
   unwrap-form
   clean-code
   file-eval
   macroexpand-all-code
   macroexpand-some
   macroexpand-n
   macroexpand-do]

  [shuriken.meta
   without-meta
   merge-meta
   preserve-meta]

  [shuriken.monkey-patch
   once
   refresh-once
   monkey-patch
   java-patch
   ; copy-class
   ; save-class
   ]

  [shuriken.namespace
   fully-qualify fully-qualified?
   unqualify
   with-ns
   import-namespace ;; TODO: deprecated
   import-namespace-vars]

  [shuriken.tree
   prepostwalk
   prepostwalk-demo
   tree
   tree-seq-breadth]

  [shuriken.reflection
   read-field
   write-field
   method
   static-method]

  [shuriken.spec
   conf either conform!]

  [shuriken.string
   adjust
   format-code
   join-lines
   join-words
   lines
   no-print
   tabulate
   truncate
   words])

(import-namespace-vars shuriken.sequential)
