 # shuriken

Small yet effective Clojure weapons.

# Usage

```clojure
[org.clojars.tristefigure/shuriken "0.5.0"]
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
  (fully-qualify 'a-symbol)        ;; 'other-namespace/a-symbol
  (fully-qualify *ns* 'o/a-symbol) ;; 'other-namespace/a-symbol
```

## Navigation

### `tree-seq-breadth`

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

## Meta

### `without-meta`

```clojure
(meta (without-meta (with-meta [1 2 3] {:metadata :abc}))) ;; nil
```


