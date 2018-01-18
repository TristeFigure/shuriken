(ns shuriken.dev
  "### Dev tools
  
  Includes a monkey patch that makes working with syntax-quote easier.
  Compare:
  
  ```clojure
  (pprint ``(a b [c]))
  => (clojure.core/seq
       (clojure.core/concat
         (clojure.core/list 'user/a)
         (clojure.core/list 'user/b)
         (clojure.core/list
         (clojure.core/apply
           clojure.core/vector
           (clojure.core/seq
           (clojure.core/concat (clojure.core/list 'user/c)))))))
  
  (require 'shuriken.dev)
  
  (pprint ``(a b [c]))
  => `(user/a user/b [user/c])
  ```
  
  This also works on unquotes:
  ```clojure
  (pprint ``(+ 1 ~'~'~n)) ; ouch
  => `(clojure.core/+ 1 ~n)
  ```
  "
  (:require [shuriken.monkey-patches.syntax-quote]))
