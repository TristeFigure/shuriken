(ns shuriken.core
  (:require [potemkin :refer [import-vars]]
            [shuriken associative
                      debug
                      flow
                      macro
                      meta
                      monkey-patch
                      namespace
                      navigation
                      sequential
                      string
                      threading
                      weaving]))

(import-vars
  [shuriken.associative
   flatten-keys deflatten-keys deep-merge index-by]
  
  [shuriken.debug
   debug]

  [shuriken.flow
   silence
   thrown?]

  [shuriken.macro
   is-form?
   wrap-form
   unwrap-form
   clean-code
   lexical-context
   stored-locals
   store-locals!
   delete-stored-locals!
   binding-stored-locals
   lexical-eval
   file-eval
   macroexpand-some
   macroexpand-n
   macroexpand-do]

  [shuriken.meta
   without-meta]

  [shuriken.monkey-patch
   monkey-patch
   only
   refresh-only]
  
  [shuriken.namespace
   fully-qualify fully-qualified?
   unqualify
   with-ns]

  [shuriken.navigation
   tree-seq-breadth
   prepostwalk
   prepostwalk-demo]

  [shuriken.sequential
   slice separate
   max-by min-by]
  
  [shuriken.string
   tabulate
   truncate]
  
  [shuriken.threading
   tap tap-> tap->>
   pp->]
  
  [shuriken.weaving
   | not|
   *| <-| ->| 
   when| if| tap|
   and| or|])
