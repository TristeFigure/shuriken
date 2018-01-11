(ns shuriken.core
  (:require [potemkin :refer [import-vars]]
            [shuriken associative
                      context
                      debug
                      destructure
                      exception
                      fn
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
  
  [shuriken.dance
   dance
   merge-dances]
  
  [shuriken.debug
   debug]
  
  [shuriken.destructure
   disentangle
   deconstruct
   restructure]

  [shuriken.exception
   silence
   thrown?]
  
  [shuriken.fn
   arities
   max-arity
   min-arity
   fake-arity]

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
   if-> if->>
   pp-> pp->>]
  
  [shuriken.weaving
   | not|
   *| <-| ->| apply| arity-comp
   when| if| tap|
   and| or|
   context|])
