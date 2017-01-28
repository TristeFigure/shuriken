(ns shuriken.core
  (:require [potemkin :refer [import-vars]]
            [shuriken associative
                      flow
                      meta
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

  [shuriken.meta
   without-meta]

  [shuriken.namespace
   fully-qualify fully-qualified?
   unqualify]

  [shuriken.navigation
   tree-seq-breadth]

  [shuriken.predicates-composer
   and? or? not?]
  
  [shuriken.sequential
   slice separate])
