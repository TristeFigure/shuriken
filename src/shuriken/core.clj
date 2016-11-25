(ns shuriken.core
  (:require [potemkin :refer [import-vars]]
            [shuriken associative
                      meta
                      namespace
                      navigation
                      predicates-composer]))

(import-vars
  [shuriken.associative
   flatten-keys deflatten-keys deep-merge]

  [shuriken.meta
   without-meta]

  [shuriken.namespace
   fully-qualify]

  [shuriken.navigation
   tree-seq-breadth]

  [shuriken.predicates-composer
   and? or? not?])
