(ns shuriken.core
  (:require [potemkin :refer [import-vars]]
            [shuriken associative
                      flow
                      macro
                      meta
                      monkey-patch
                      namespace
                      navigation
                      predicates-composer
                      sequential]))

(import-vars
  [shuriken.associative
   flatten-keys deflatten-keys deep-merge index-by]

  [shuriken.flow
   silence
   thrown?]

  [shuriken.macro
   clean-code
   macroexpand-some
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

  [shuriken.predicates-composer
   and? or? not?]

  [shuriken.sequential
   slice separate])
