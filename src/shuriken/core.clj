(ns shuriken.core
  (:require [potemkin :refer [import-vars]]
            [shuriken associative
                      context
                      debug
                      destructure
                      exception
                      macro
                      meta
                      monkey-patch
                      multi
                      namespace
                      navigation
                      sequential
                      spec
                      string]))

(import-vars
  [shuriken.associative
   map-keys map-vals
   flatten-keys deflatten-keys deep-merge
   index-by unindex
   merge-with-plan]

  [shuriken.context
   contexts
   context!
   context
   delete-context!
   binding-context
   lexical-map
   lexical-context
   lexical-eval
   letmap]

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

  [shuriken.multi
   call-method
   super-method]

  [shuriken.namespace
   fully-qualify fully-qualified?
   unqualify
   with-ns
   import-namespace]

  [shuriken.navigation
   tree-seq-breadth
   prepostwalk
   prepostwalk-demo]

  [shuriken.sequential
   slice separate
   max-by min-by]

  [shuriken.spec
   conf either conform!]

  [shuriken.string
   adjust
   format-code
   join-lines
   lines
   no-print
   tabulate
   truncate])
