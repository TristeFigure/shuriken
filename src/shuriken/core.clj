(ns shuriken.core
  (:require [potemkin :refer [import-vars]]
            [shuriken associative
                      context
                      dance
                      debug
                      destructure
                      exception
                      fn
                      macro
                      meta
                      monkey-patch
                      multi
                      namespace
                      navigation
                      sequential
                      string
                      threading
                      weaving]))

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

  [shuriken.dance
   dance
   merge-dances
   step
   step-indexed
   empty-dance
   depth-dance
   indexed-dance
   path-dance
   leaf-collecting-dance
   splice
   splicing-dance
   *default-dance*
   break-dance
   backtrack]

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

  [shuriken.string
   tabulate
   truncate]

  [shuriken.threading
   defthreading
   tap    tap->    tap->>
   if->   if->>
   when-> when->>
   and->  and->>
   or->   or->>
   pp->   pp->>
   <-]

  [shuriken.weaving
   | ?| not|
   *| <-| ->| apply| arity-comp
   when| if| tap|
   and| or|
   context|])
