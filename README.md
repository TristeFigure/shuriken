# shuriken

Small yet effective Clojure weapons.

<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/1/1a/Yamato_Takeru_at_16-crop.jpg">
</p>

<p align="center">
  <i>
    Yamato Takeru dressed as a maidservant, preparing to kill the Kumaso leaders.
  </i>
</p>

# Usage

```clojure
[org.clojars.tristefigure/shuriken "0.14.12"]
```


```clojure
(ns my-ns
  (:require [shuriken.core :refer :all]))
```

## [API doc](https://tristefigure.github.io/shuriken/index.html)

# Grown-ups

Libraries that were originally part of shuriken.

- [lexikon](https://github.com/TristeFigure/lexikon): Reify, manipulate and replay the lexical environment in Clojure.
- [arity](https://github.com/TristeFigure/arity): Get and fake arities of Clojure functions.
- [threading](https://github.com/TristeFigure/threading): A Clojure threading macros library as sobber as its name.
- [weaving](https://github.com/TristeFigure/weaving): Weaving is to lambdas what threading is to s-expressions.
- [methodman](https://github.com/TristeFigure/methodman): Shaolin moves for Clojure methods.
- [dance](https://github.com/TristeFigure/dance): Advanced tree walking in Clojure.

## Associative structures

#### The boring

- `map-keys` & `map-vals`
- `submap?`

#### `flatten-keys`

```clojure
(flatten-keys {:a {:b {:c :x
                       :d :y}}})
;; {[:a :b :c] :x
;;  [:a :b :d] :y}
```

#### `deflatten-keys`

```clojure
(deflatten-keys {[:a :b :c] :x
                 [:a :b :d] :y})
;; {:a {:b {:c :x
;;          :d :y}}}
```

#### `deep-merge`

```clojure
(deep-merge {:x {:a :a  :b :b  :c :c}}
            {:x {:a :aa :b :bb}}
            {:x {:a :aaa}})

;; {:x {:a :aaa  :b :bb  :c :c}}
```

#### `index-by`

```clojure
(def ms [{:a 1 :b 2} {:a 3 :b 4} {:a 5 :b 4}])

(index-by :a ms)
;; {1 {:a 1 :b 2}
;;  3 {:a 3 :b 4}
;;  5 {:a 5 :b 4}}

(index-by :b ms)
;; clojure.lang.ExceptionInfo (Duplicate entries for key 4)

(index-by :b (fn [key entries]
               (last entries))
          ms)
;; {2 {:a 1 :b 2}
;;  4 {:a 5 :b 4}}
```

#### `split-map`

```clojure
(let [m {:a 1 :b 2 :c 3 :d 4}]
  (split-map m [:a :b])       ;; [{:a 1 :b 2} {:c 3 :d 4}]
  (split-map m [:a :b] [:c])) ;; [{:a 1 :b 2} {:c 3} {:d 4}]
```

#### `map-difference`

```clojure
(let [m {:a 1 :b 2 :c 3}]
  (map-difference m {:a :x})          ;; {:b 2 :c 3}
  (map-difference m {:a :x} {:b :x})) ;; {:c 3}
```

#### `getsoc`

Works like `get` if the key is present in the hash, else works like assoc`.
Returns a vector of the form `[get-or-stored-value new-coll]`.

```clojure
(getsoc {:a 1} :a (constantly 2)) ;; => [1 {:a 1}] 
(getsoc {} :a (constantly 2)))    ;; => [2 {:a 2}]
```

## Exceptions

#### `silence`

```clojure
(silence ArithmeticException (/ 1 0))
;; => nil

(silence [ArithmeticException]
  (do (println "watch out !")
      (/ 1 0)))
;; watch out !
;; => nil

(silence "Divide by zero" (/ 1 0))
;; => nil

(silence :substitute
         (fn [x]
           (isa? (class x) ArithmeticException))
         (/ 1 0))
;; => :substitute
```

#### `thrown?`

```clojure
(thrown? ArithmeticException (/ 1 0))
;; => true

(thrown? "Divide by zero" (/ 1 0))
;; => true

(thrown? #{ArithmeticException} (/ 1 1))
;; => false

(thrown? (fn [x]
           (isa? (class x) ArithmeticException))
         (throw (IllegalArgumentException. "my-error")))
;; raises:
;;   IllegalArgumentException my-error

(thrown? {:type :oops}
         (throw (ex-info "Oops" {:type :oops :value :abc})))
;; => true
```

## Sequential structures

#### `get-nth`, `get-nth-in`, `assoc-nth`,  `assoc-nth-in`, `update-nth` & `update-nth-in`

Respectively like `get`, `get-in`, `assoc` etc... but also work on lists.

#### `slice`

```clojure
(let [coll [1 1 0 1 0 0 1 1]]
  ;; the default
  (slice zero? coll) ;; by default, :include-delimiter false, include-empty true
  ;; ((1 1) (1) (1 1))
  
  (slice zero? coll :include-empty true)
  ;; ((1 1) (1) () (1 1))
  
  (slice zero? coll :include-delimiter :left)
  ;; ((1 1) (0 1) (0 1 1))
  
  (slice zero? coll :include-delimiter :right)
  ;; ((1 1 0) (1 0) (1 1))
  
  (slice zero? coll :include-delimiter :right :include-empty true)
  ;; ((1 1 0) (1 0) (0) (1 1))
  )
```

#### `separate`

Returns a vector of `[(filter pred coll) (remove pred coll)]`.

```clojure
(let [coll [1 1 0 1 0 0 1 1 0]]
  (separate zero? coll)
  ;; [(1 1 1 1 1) (0 0 0 0)]
  )
```

#### `order`

Order a sequence with constraints.

```clojure
(order [1 2 3] {2 1           3 :all})
(order [1 2 3] [[2 1]         [3 :all]])
(order [1 2 3] [[2 :before 1] [:all :after 3]])
(order [1 2 3] [[2 :> 1]      [:all :< 3]])
;; (3 2 1)
```

### `takes`

Split a sequence in subsequence of predetermined length.

```clojure
(takes [1 2 3] [:a :b])                ;; => ((:a) (:b))
(takes [1 2 3] [:a :b :c])             ;; => ((:a) (:b :c))
(takes [1 2 3] [:a :b :c :d :e :f])    ;; => ((:a) (:b :c) (:d :e :f))
(takes [1 2 3] [:a :b :c :d :e :f :g]) ;; => ((:a) (:b :c) (:d :e :f) (:g))
(takes [0 0 1 0 2] [:a :b :c :d :e])   ;; => (() () (:a) () (:b :c) (:d :e))
```

### `insert-at`

Insert an item into a list or a vector.

```clojure
(insert-at [1 2 3]  0 :x) ;; => [:x 1 2 3]
(insert-at '(1 2 3) 1 :x) ;; => '(1 :x 2 3)
(insert-at '(1 2 3) 3 :x) ;; => '(1 2 3 :x)
(insert-at [1 2 3]  4 :x) ;; => java.lang.IndexOutOfBoundsException
(insert-at [1 2 3] -1 :x) ;; => java.lang.IndexOutOfBoundsException
```

## Macro

#### Working with forms

```clojure
(is-form? 'a 1)       ; false
(is-form? 'a '[a :z]) ; false
(is-form? 'a '(a :z)) ; true

(wrap-form 'a :z)                      ; (a :z)
(->> :z (wrap-form 'a) (wrap-form 'a)) ; (a :z)

(unwrap-form 'a '(a :z))                        ; a
(->> '(a :z) (unwrap-form 'a) (unwrap-form 'a)) ; a
```

#### `clean-code`

Recursively unqualifies qualified code in the provided form.

```clojure
(clean-code `(a (b c)))
;; (a (b c))
```

#### `file-eval`

Evaluate code in a temporary file via load-file in the local lexical
context. Keep the temporary file aside if an error is raised, deleting it on
the next run.

```clojure
(let [a 1]
  (file-eval '(+ 1 a)))
```

Code evaluated this way will be source-mapped in stacktraces.

#### `macroexpand-n`

Iteratively call macroexpand-1 on form n times.

```clojure
(defmacro d [x] x)
(defmacro c [x] `(d ~x))
(defmacro b [x] `(c ~x))
(defmacro a [x] `(b ~x))

(macroexpand-n 2 '(a 1))
; (my-ns/c 1)
```

#### `macroexpand-some`

Recursively macroexpand forms whose first element match a filter.
Symbols are passed to filter unqualified.

```clojure
(defmacro by [code]
  `(inc ~code))

(defmacro az [code]
  `(by ~code))

(macroexpand-some '#{az} '(az (+ 1 2)))
; => (user/by (+ 1 2))
```

#### `macroexpand-do`

```clojure
(defmacro abc []
  `(println "xyz"))

(macroexpand-do (abc))

; -- Macro expansion --
; (clojure.core/println "xyz")

; -- Running macro --
; xyz
```

or alternatively:

```clojure
(macroexpand-do MODE
  expr)
```

Where `MODE` has the following meaning:

| `MODE`                        | expansion                        |
|-------------------------------|----------------------------------|
| `nil` (the default)           | `macroexpand`                    |
| `:all`                        | `clojure.walk/macroexpand-all`   |
| a number n                    | iterate `macroexpand-1` n times  |
| anything else                 | a predicate to `macroexpand-some`|

###### Tracking exceptions in code generated by macros

To allow for better tracking of exceptions in code generated by macros,
`macroexpand-do` evaluates the expansion of the macro expression in a temporary
file that is kept aside for inspection and appears in the stacktrace of raised
exceptions. If no exception is raised, the file is deleted.

## Meta

#### `without-meta`

```clojure
(meta (without-meta (with-meta [1 2 3] {:metadata :abc}))) ;; nil
```

## Monkey-patch

#### `monkey-patch`

```clojure
(monkey-patch :perfid-incer clojure.core/+ [original & args]
  (inc (apply original args)))
```
  
Supports reload. Name and target can be vars or quoted symbols.

#### `only`

Ensures the code is executed only once with respect to the associated name.
name must be a symbol, quoted or not.

```clojure
(only 'only-once (println "printed once"))
(only 'only-once (println "printed once"))

;; printed once
```

#### `refresh-only`

```clojure
(only 'a (println "a"))
;; a

(only 'a (println "a"))
;; prints nothings

(refresh-only 'a)
(only 'a (println "a"))
;; a
```

## Namespace

#### `fully-qualify`

Returns the fully-qualified form of the symbol as if resolved from within a
namespace.

```clojure
(fully-qualified? 'IRecord)      ; => clojure.lang.IRecord
(fully-qualified? 'my-var)       ; => my-ns/my-var
(fully-qualified? 'alias/my-var) ; => actual.namespace/my-var
```

#### `fully-qualified?`
Returns true if the symbol constitutes an absolute reference.

```clojure
(fully-qualified? 'clojure.lang.IRecord) ; => true
(fully-qualified? 'my-ns/my-var)         ; => true
(fully-qualified? 'alias/my-var)         ; => false
```

#### `unqualify`

```clojure
(unqualifiy 'clojure.lang.IRecord)       ; => IRecord
(unqualifiy 'my-ns/my-var)               ; => my-var
(unqualifiy 'alias/my-var)               ; => alias/my-var
(unqualifiy 'some.path.Class/staticMeth) ; => Class/staticMeth
```

#### `with-ns`

Like `in-ns` but with the scope of a `let` or a `binding`.

```clojure
(with-ns 'my-namespace ;; or (find-ns 'my-namespace)
  (def number 123))

(println my-namespace/number)
;; 123
```

## Navigation

#### `tree-seq-breadth`

```clojure
(let [tree {:a {:d {:j :_}
                :e {:k :_}}
            :b {:f {:l :_}
                :g {:m :_}}
            :c {:h {:n :_}
                :i {:o :_}}}
      keys-only #(->> % (remove #{:_}) (mapcat keys))]
  (keys-only (tree-seq map? vals tree))
  ;; (:a :b :c :d :e :j :k :f :g :l :m :h :i :n :o)
  (keys-only (tree-seq-breadth map? vals tree))
  ;; '(:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o)
  )
```

#### `prepostwalk`

A combination of clojure.walk's prewalk and postwalk. Recursively modifies
form with pre-fn before descending further into the structure and
then with post-fn after going up.

```clojure
(defn inc-it [x]
  (if (number? x) (inc x) x))

(defn it-times-two [x]
  (if (number? x) (* 2 x) x))

(def data
  [1 2 {3 4 5 6}])

(println (prepostwalk inc-it it-times-two data))
;; [4 6 {8 10, 12 14}]
(println (prepostwalk it-times-two inc-it data))
;; [3 5 {7 9, 11 13}]
```

Comes equipped with `prepostwalk-demo`

## Dev

For all features listed below:
```clojure
(require 'shuriken.dev)
```

#### `shuriken.monkey-patches.syntax-quote`

Before
```clojure
``abc
;; (quote my-ns/abc)

(pprint ``abc)
;; 'my-ns/abc
```

After
```clojure
(require 'shuriken.monkey-patches.syntax-quote)

``abc
;; (clojure.core/syntax-quote my-ns/abc)

(pprint ``abc)
;; `my-ns/abc

(def args [1 2 3])

(pprint (syntax-quote `(abc ~@args)))
;; `(my-ns/abc 1 2 3)

(pprint (syntax-quote `(abc ~'~@args)))
;; `(my-ns/abc ~@args)
```

But above all it prevents this:
```clojure
(pprint ``(do (fn1 arg1 arg2) (fn2 arg3 arg4)))

(clojure.core/seq
 (clojure.core/concat
  (clojure.core/list 'do)
  (clojure.core/list
   (clojure.core/seq
    (clojure.core/concat
     (clojure.core/list 'user/fn1)
     (clojure.core/list 'user/arg1)
     (clojure.core/list 'user/arg2))))
  (clojure.core/list
   (clojure.core/seq
    (clojure.core/concat
     (clojure.core/list 'user/fn2)
     (clojure.core/list 'user/arg3)
     (clojure.core/list 'user/arg4))))))
```


And pprints this instead:
```clojure
(pprint ``(do (fn1 arg1 arg2) (fn2 arg3 arg4)))

`(do (my-ns/fn1 my-ns/arg1 my-ns/arg2) (my-ns/fn2 my-ns/arg3 my-ns/arg4))
```
