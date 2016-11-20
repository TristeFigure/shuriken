(ns shuriken.core
  (:require [potemkin :refer [import-vars]]
            [shuriken predicates-composer
                      meta
                      associative
                      namespace]))

(import-vars
  [shuriken.predicates-composer
   and? or? not?]
  [shuriken.meta
   without-meta]
  [shuriken.associative
   flatten-keys deflatten-keys deep-merge]
  [shuriken.namespace
   fully-qualify])
