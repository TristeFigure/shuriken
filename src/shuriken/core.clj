(ns shuriken.core
  (:require [potemkin :refer [import-vars]]
            [shuriken associative
                      debug
                      destructure
                      exception
                      lazy
                      macro
                      meta
                      monkey-patch
                      namespace
                      navigation
                      reflection
                      sequential
                      spec
                      string]))

(import-vars
  [shuriken.associative
   map-keys map-vals
   flatten-keys deflatten-keys deep-merge
   index-by unindex
   merge-with-plan
   split-map
   map-difference
   map-intersection
   submap?
   getsoc]

  [shuriken.debug
   debug
   debug-print]

  [shuriken.destructure
   disentangle
   deconstruct
   restructure]

  [shuriken.exception
   silence
   thrown?]

  [shuriken.lazy
   deep-doall]

  [shuriken.macro
   is-form?
   wrap-form
   unwrap-form
   clean-code
   file-eval
   macroexpand-some
   macroexpand-n
   macroexpand-do]

  [shuriken.meta
   without-meta]

  [shuriken.monkey-patch
   only
   refresh-only
   monkey-patch
   java-patch]

  [shuriken.namespace
   fully-qualify fully-qualified?
   unqualify
   with-ns
   import-namespace]

  [shuriken.navigation
   tree-seq-breadth
   prepostwalk
   prepostwalk-demo]

  [shuriken.reflection
   read-field
   method
   static-method]

  [shuriken.sequential
   get-nth get-nth-in
   assoc-nth assoc-nth-in
   update-nth update-nth-in
   insert-at
   slice separate
   max-by min-by
   order
   takes]

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
