# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [0.14.33] - 2019-01-30
### Fixed:
- `spec`: `:single-body` info on `args+bodies` spec.
- `pprint-fn` works.

## [0.14.32] - 2019-01-12
### Enhanced:
- `sequential`: additional arguments for:
  - `assoc-nth` & `update-nth`: `not-found`.
  - `assoc-nth-in` & `update-nth-in`: `not-found-f`.

## [0.14.31] - 2018-12-24
### Fixed:
- `sequential`: edge cases.

## [0.14.30] - 2018-12-24
### Added:
- `pprint-fn` monkey-patch.

## [0.14.29] - 2018-12-23
### Added:
- `fn-source` monkey-patch.

## [0.14.28] - 2018-10-28
### Fixed
- `macro`: `macroexpand-do` accepts quoted and unquoted code.
- `monkey-patch`: guard against unexisting sources in `define-again`.
- `monkey-patches.pprint-meta`: handle `clojure.lang.IMeta` like vars.
- `namespace`: better support of class names for `unqualify` & friends.

## [0.14.27] - 2018-10-27
### Fixed
- `namespace`: `unqualify`, `fully-qualify` and `fully-qualified?`
  now handle symbols starting with a dot.
- `monkey-patches.pprint-meta`:
  - works recursively.
  - handles shorthands (`^Tag` & `^:boolean-flag`).

## [0.14.26] - 2018-10-26
### Fixed
- bumped `dance` to `0.1.2`.

## [0.14.25] - 2018-10-26
### Added
- `monkey-patches.pprint-meta`.

## [0.14.24] - 2018-10-06
### Added
- `spec`: `::reify-form` & `::deftype-form`.

## [0.14.23] - 2018-10-06
### Added
- `associative`: `filter-keys`, `filter-vals`, `remove-keys` & `remove-vals`.
- `spec`: `::defn-form` (same as `::defmacro-form`).

### Fixed:
- `sequential`: `order` now ignores constraints about absent elements and
  raises an exception if the collection is not `distinct?`.

## [0.14.22] - 2018-09-30
### Changed
- `sequential`: invert the meaning of `:>` and `:<` in `order`.

## [0.14.21] - 2018-09-30
### Fixed
- `macro`: remove debug statement in `macroexpand-do`.

## [0.14.20] - 2018-09-29
### Enhanced
- `exception`: `silence` and `thrown?` now support regex patterns.

## [0.14.19] - 2018-09-24
### Fixed:
- `sequential`: fix `assoc-nth` on `nil` and `assoc-nth-in` on sequences.

## [0.14.18] - 2018-09-24
### Fixed:
- `sequential`: fix `assoc-nth` and `assoc-nth-in`.

## [0.14.17] - 2018-09-18
### Added:
- `associative`: `map-intersection`.

## [0.14.16] - 2018-09-16
### Fixed:
- `associative`: `map-keys` and `map-vals` order on sequences.

## [0.14.15] - 2018-09-15
### Fixed:
- `associative`: `map-keys` and `map-vals` order on lists.

## [0.14.14] - 2018-09-14
### Fixed:
- `debug`: fix `debug-print` again.

## [0.14.13] - 2018-09-14
### Fixed:
- `debug`: `debug-print` now handles multi-line results properly.

## [0.14.12] - 2018-09-05
### Added
- `associative`: `getsoc`.

## [0.14.11] - 2018-09-04
### Fixed
- `spec`: somes specs use `seq?` insted of `list?` as a more general predicate.

## [0.14.10] - 2018-09-04
### Added
- `spec`: `:shuriken.spec/leftn-spec` and `:shuriken.spec/letfn-specs`

## [0.14.9] - 2018-09-04
### Added
- `spec`: `:shuriken.spec/fn-form` spec.

### Changed
- `spec`: spec `:shuriken.spec/macro-definition` renamed to
  `:shuriken.spec/defmacro-form`.

## [0.14.8] - 2018-09-02
### Added
- `sequential`: `insert-at`.

## [0.14.7] - 2018-09-02
### Added
- `sequential`: in addition to `get-nth-in` & `assoc-nth-in`, added:
  - `get-nth` & `assoc-nth`.
  - `update-nth` & `update-nth-in`.

## [0.14.6] - 2018-08-30
### Fixed
- `sequential`: `takes` handles `0`-sized chunks.

## [0.14.5] - 2018-08-16
### Added
- `sequential`: `takes`.

## [0.14.4] - 2018-07-11
### Added
- `sequential`: `order` to order a sequence with constraints.

## [0.14.3] - 2018-06-15
### Fixed
- `map-keys` & `map-vals`: will preserve the class of the passed associative
  structure.

## [0.14.2] - 2018-05-27
### Added
- `string`: `join-words`.
- New namespace `lazy`: `deep-doall`.

### Removed
- exported `context` to [lexikon](https://github.com/TristeFigure/lexikon).

## [0.14.1] - 2018-05-21
### Added
- `string`: `words`.

## [0.14.0] - 2018-05-20
### Removed
- exported `multi` to [methodman](https://github.com/TristeFigure/methodman).

## [0.13.34] - 2018-05-20
### Added
- `multi`: `multi-name`.

## [0.13.33] - 2018-05-20
### Fixed
`multi`: fix bug in bug fix of `super-method`.

## [0.13.32] - 2018-05-20
### Fixed
`multi`: error reporting in `super-method`.

## [0.13.31] - 2018-05-19
### Added
- `multi`: `defmethods`.

## [0.13.30] - 2018-05-13
### Added
- `associative`: `submap?`.
- `exception`: `silence` and `thrown?` can match and ExceptionInfo ex-data
  with a map (works with `submap?`).
- `reflection`: `read-field`, `method` & `static-method`.
- `multi`:
    - `method`.
    - `augmentable-multi` & `augment-method`.
    - `extendable-multi` & `extend-method`.

## [0.13.29] - 2018-05-10
### Added
- `exception`: `silence` and `thrown` can match the exception message if
  passed a string.

## [0.13.28] - 2018-05-10
### Added
- `associative`: `split-map` & `map-difference`.

## [0.13.27] - 2018-05-01
### Removed
- Externalized `threading`, `weaving`, `dance` and `fn` (this latter as
  `arity`) into distinct libs of their own.

### Added
- `associative`: a plan to `merge-plan` can be a map as well as a function.

## [0.13.26] - 2018-04-30
### Fixed
- Exposed `spec` namespace.

## [0.13.25] - 2018-04-30
### Fixed
- `string`: exposed `adjust`, `format-code`, `join-lines`, `lines` &
  `no-print`.
  - `debug`: exposed `debug-print`.

## [0.13.24] - 2018-04-30
### Added
- Exposed test utils in `shuriken.test`. Not included in `shuriken.core`.

## [0.13.23] - 2018-04-28
### Added
- `threading`: `and->`, `and->>`, `or->` & `or->>`

## [0.13.22] - 2018-04-28
### Added
- `associative`: `map-keys` & `map-vals`.

## [0.13.21] - 2018-04-28
### Added
- `sequential`: `get-nth-in` & `assoc-nth-in`.

## [0.13.20] - 2018-04-28
### Fixed
- `multi`: in `super-method`, use the default dispatch val of the multi
  instead of `:default`.

## [0.13.19] - 2018-04-28
### Added
- `multi`: `super-method`.

### Fixed
- `multi`: `call-multi` renamed to `call-method`.

## [0.13.18] - 2018-04-27
### Added
- macro:
  - `is-form?`, `wrap-form` & `unwrap-form`.
  - `lexical-context`.
  - `stored-locals!`, `binding-stored-locals` & more.
  - `lexical-eval`.
- multi:
  - `call-multi`.
- namespace `predicates-composer` renamed to `weaving`:
  - `|` (`constantly`).
  - `*|`, `<-|` & `->|`.
  - `when|`, `if|` & `tap|`.
  - `and?`, `or?`, `not?` renamed to `and|`, `or|`, `not|`.
- debug: `debug`.
- sequential: `max-by` & `min-by`.
- string: `tabulate` & `truncate`.
- threading:
  - `defthreading`
  - `if->` & `if->>`.
  - `when->` & `when->>`.
  - `pp->`.
  - `<-`.

### Enhanced
- macro: `file-eval` uses the local lexical context.

### Fixed
- macro: fix `clean-code` and `macroexpand-some`.

## [0.13.8] - 2017-11-28
### Enhanced
- macro:
  - `macroexpand-do` accepts predicates.
  - `file-eval` has better file output.

## [0.13.7] - 2017-11-28
### Fixed
- `macro`: errors from `file-eval` display both the extended message with the
  temporary file path and the actual error stacktrace.

## [0.13.6] - 2017-11-28
### Fixed
- `macroexpand-do`: dumps the expanded form at the file-eval stage.

### Enhanced
- `tap` threads the initial expr to the forms in its body that are threading
  forms.

## [0.13.5] - 2017-11-28
### Added
- `macro`: `file-eval`, `macroexpand-n`.
- `threading`: `tap`, `tap->` & `tap->>`.

### Enhanced
- `macro`: `macroexpand-do` use `file-eval` to evaluate the expression's
  expansion. Useful to track exceptions raised by macro-generated code.

### Fixed
- `macro`: `macroexpand-do` with a number calls `macroexpand-1` the right
  number of times.

## [0.13.4] - 2017-11-27
### Added
- `macro`: `macroexpand-some`.

### Enhanced
- `macro`: `macroexpand-do` supports n iterations of `macroexpand-1` as well
  as `macroexpand-some`.

### Fixed
- `namespace`: `fully-qualify`, `fully-qualified?` and `unqualify` support
  static method symbols like `some.Class/staticMethod`.

## [0.13.3] - 2017-11-27
### Added
- `navigation`: `prepostwalk` & `prepostwalk-demo`.
- `monkey-patch`: `monkey-patch`, `only` & `refresh-only`.
- `macro`: `clean-code`.

### Removed
- `namespace`: `once-ns`.

## [0.13.2] - 2017-10-29
### Added
- `shuriken.dev` namespace with `shuriken.monkey-patches.syntax-quote`.

### Fixed
- Remove `shuriken.monkey-patches.syntax-quote` from `shuriken.core`.
- `shuriken.monkey-patches.syntax-quote` changes `clojure.lang.LispReader`
  to use `clojure.tools.reader/read`.

## [0.13.1] - 2017-10-28
### Added
- Monkey-patch clojure:
  - New `pprint` translations for reader macro `~@`.

## [0.13.0] - 2017-10-22
### Added
- Execute code in another namespace with `with-ns`.
- Ensure a namespace is loaded only once with `once-ns` even using `use`
  or `require` with `:reload` `:reload-all`.
- Monkey-patch clojure to:
  - Use clojure.tools.reader `read` and `read-string`.
  - Introduce `syntax-quote`.
  - New `pprint` translations for reader macro ```.

## [0.12.2] - 2017-03-25
### Fixed
- `macro`: beautify output.
- CHANGELOG: some dates were not in ISO yyyy-mm-dd format.

## [0.12.1] - 2017-03-25
### Fixed:
  - `macro`: fixed a string in `macroexpand-do`.

## [0.12.0] - 2017-03-25
### Added
  - `macro`: `macroexpand-do`.

## [0.9.0] - 2017-01-28
### Added
  - `flow`: `silence`, `thrown?`.

## [0.8.0] - 2017-01-22
### Added
  - `sequential`: `separate`.

## [0.7.0] - 2017-01-16
### Added
  - `sequential`: `slice`.

## [0.6.0] - 2016-12-02
### Added
  - `associative`: `index-by`.

## [0.5.0] - 2016-11-27
### Added
  - `namespace`: `fully-qualified?`, `unqualify`.

## [0.4.0] - 2016-11-26
### Added
  - `navigation`: `tree-seq-breadth`.

## [0.3.0] - 2016-11-20
### Added
  - `associative`: `flatten-keys`, `deflatten-keys`, `deep-merge`.
  - `meta`: `without-meta`.

## [0.2.0] - 2016-11-20
### Added
  - `namespace`: `fully-qualify`.

## [0.1.0] - 2016-11-20
### Added
  - predicate composers: `and?`, `or?` & `not?`.
