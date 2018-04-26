(ns shuriken.monkey-patches.syntax-quote
  (:require [shuriken.namespace :refer [with-ns]]
            [shuriken.monkey-patch :refer [monkey-patch java-patch
                                           require-from-dependent-namespaces
                                           define-again]]
            [clojure.pprint]))

;; Step 0: substitute Clojure's native reader with that from
;; clojure.tools.reader
; (monkey-patch :substitute-clojure-read
;   clojure.core/read
;   [original & args]
;   (apply clojure.tools.reader/read args))

; (monkey-patch :substitute-clojure-read-string
;   clojure.core/read-string
;   [original & args]
;   (apply clojure.tools.reader/read-string args))

; (monkey-patch :change-tools-reader-read-syntax-quote
;   clojure.tools.reader/read-syntax-quote
;   [original & args]
;   (list 'from-syntax-quote (apply original args)))

; ;; Also change the reader used by Clojure's Compiler to use
; ;; clojure.tools.reader/read
; (java-patch [clojure.lang.LispReader "read"
;              [java.io.PushbackReader "boolean" Object "boolean" Object]]
;   :replace
;   [reader eof-is-error eof-value _is-recursive opts]
;   (clojure.tools.reader/read
;     (merge {:eofthrow eof-is-error
;             :eof eof-value}
;            opts)
;     reader))


;; Step 1: Tag forms expanded with syntax-quote
(with-ns 'clojure.core
  (defn from-syntax-quote [x]
    x))

(java-patch [clojure.lang.LispReader$SyntaxQuoteReader "syntaxQuote" [Object]]
  :after
  [result & args]
  (list 'from-syntax-quote result))


;; Step 2: Add (from-syntax-quote x) -> `(x) translation into clojure.pprint
;;         and translate (unquote-splicing args) to ~@args as well.
(monkey-patch :pprint-from-syntax-quote-as-reader-macro
  clojure.pprint/pprint-reader-macro
  [original alis]
  (or (original alis)
      (when (and (contains? '#{clojure.core/from-syntax-quote from-syntax-quote}
                            (first alis))
                 (= 2 (count alis)))
        (.write ^java.io.Writer *out* "`")
        (clojure.pprint/write-out (eval (second alis)))
        true)))

(monkey-patch :add-unquote-splicing-to-pprint-reader-macros
  clojure.pprint/reader-macros
  [original]
  (assoc original
    'clojure.core/unquote-splicing "~@"))

;; Reload functions using pprint-reader-macro.
(define-again 'clojure.pprint/pprint-list)
(define-again 'clojure.pprint/pprint-code-list)

(with-ns 'clojure.pprint
  (use-method simple-dispatch clojure.lang.ISeq pprint-list)
  (use-method code-dispatch   clojure.lang.ISeq pprint-code-list))


;; Step 3: reload clojure.core from dependent namespaces as well as
;;         clojure.pprint
(require-from-dependent-namespaces
  '[clojure.core :refer [from-syntax-quote] :reload true])

(require-from-dependent-namespaces
  '[clojure.pprint :reload true])
