# shuriken

Small yet effective Clojure weapons.

## Predicates composer

```clojure
(filter (and? (or? is-old-enough?
                   is-vip?)
              (not? faint-hearted?)))
```
