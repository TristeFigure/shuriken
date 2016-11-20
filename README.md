# shuriken

Small yet effective Clojure weapons.

# Usage

```clojure
[org.clojars.tristefigure/shuriken "0.3.0"]
```


```clojure
(ns my-ns
  (:require [shuriken.core :refer :all]))
```

## Predicates composer

```clojure
(filter (and? (or? is-old-enough?
                   is-vip?)
              (not? faint-hearted?)))
```

## Associative structures

### `flatten-keys`

```clojure
(flatten-keys {:a {:b {:c :x
                       :d :y}}})
;; {[:a :b :c] :x
;;  [:a :b :d] :y}
```

### `deflatten-keys`

```clojure
(deflatten-keys {[:a :b :c] :x
                 [:a :b :d] :y})
;; {:a {:b {:c :x
;;          :d :y}}}
```

### `deep-merge`

```clojure
(deep-merge {:x {:a :a  :b :b  :c :c}}
            {:x {:a :aa :b :bb}}
            {:x {:a :aaa}})

;; {:x {:a :aaa  :b :bb  :c :c}}
```

## Namespace

### `fully-qualify`

```clojure
  (fully-qualify 'a-symbol)          ;; 'other-namespace/a-symbol
  (fully-qualify *ns* 'o/a-symbol) ;; 'other-namespace/a-symbol
```

## Meta

### `without-meta`

```clojure
(meta (without-meta (with-meta [1 2 3] {:metadata :abc}))) ;; nil
```


