# shuriken

Small yet effective Clojure weapons.

# Usage

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

## Other
```clojure
(fully-qualify 'a-symbol)
(fully-qualify *ns* 'other-ns/a-symbol)
```
