# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

# [0.13.4] - 2017-11-27
## Added
- macro: `macroexpand-some`.

## Enhanced
- macro: `macroexpand-do` supports n iterations of `macroexpand-1` as well
  as `macroexpand-some`.

## Fixed
- namespace: `fully-qualify`, `fully-qualified?` and `unqualify` support
  static method symbols like `some.Class/staticMethod`.

# [0.13.3] - 2017-11-27
## Added
- navigation: `prepostwalk` & `prepostwalk-demo`.
- monkey-patch: `monkey-patch`, `only` & `refresh-only`.
- macro: `clean-code`.

## Removed
- namespace: `once-ns`.

# [0.13.2] - 2017-10-29
## Added
- `shuriken.dev` namespace with `shuriken.monkey-patches.syntax-quote`.

## Fixed
- Remove `shuriken.monkey-patches.syntax-quote` from `shuriken.core`.
- `shuriken.monkey-patches.syntax-quote` changes `clojure.lang.LispReader`
  to use `clojure.tools.reader/read`.

# [0.13.1] - 2017-10-28
## Added
- Monkey-patch clojure:
  - New `pprint` translations for reader macro `~@`.

# [0.13.0] - 2017-10-22
## Added
- Execute code in another namespace with `with-ns`.
- Ensure a namespace is loaded only once with `once-ns` even using `use`
  or `require` with `:reload` `:reload-all`.
- Monkey-patch clojure to:
  - Use clojure.tools.reader `read` and `read-string`.
  - Introduce `syntax-quote`.
  - New `pprint` translations for reader macro ```.

## [0.12.2] - 2017-03-25
## Fixed
- macro: beautify output.
- CHANGELOG: some dates were not in ISO yyyy-mm-dd format.

## [0.12.1] - 2017-03-25
## Fixed:
  - macro: fixed a string in `macroexpand-do`.

## [0.12.0] - 2017-03-25
## Added
  - macro: `macroexpand-do`.

## [0.9.0] - 2017-01-28
## Added
  - flow: `silence`, `thrown?`.

## [0.8.0] - 2017-01-22
## Added
  - sequential: `separate`.

## [0.7.0] - 2017-01-16
## Added
  - sequential: `slice`.

## [0.6.0] - 2016-12-02
## Added
  - associative: `index-by`.

## [0.5.0] - 2016-11-27
## Added
  - namespace: `fully-qualified?`, `unqualify`.

## [0.4.0] - 2016-11-26
## Added
  - navigation: `tree-seq-breadth`.

## [0.3.0] - 2016-11-20
### Added
  - associative: `flatten-keys`, `deflatten-keys`, `deep-merge`.
  - meta: `without-meta`.

## [0.2.0] - 2016-11-20
### Added
  - namespace: `fully-qualify`.

## [0.1.0] - 2016-11-20
### Added
  - predicate composers: `and?`, `or?` & `not?`.
