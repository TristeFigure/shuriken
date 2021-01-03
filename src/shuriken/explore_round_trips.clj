(ns clojure.core
  (:use clojure.pprint)
  (:require [clojure.math.combinatorics :refer [combinations selections]]))

;; Proposal for fuller round-trips between clojure.core's
;; name, symbol and keyword functions with strings, symbols,
;; keywords, vars and namespaces


;; NOTE: requires [org.clojure/clojure "1.10.0-master-SNAPSHOT"]
;; Code & instructions  @ https://github.com/clojure/clojure

;; To run: (load "my_project/explore_round_trips")


;; First, some tools to test round-trips between functions
(def a-var)
(create-ns 'a-namespace)

(defn get-sym [x]
  (if (var? x)
    (.sym x)
    x))

(def test-vals
  {String                 "a-string"
   clojure.lang.Keyword   :a-keyword
   clojure.lang.Symbol    'a-symbol
   clojure.lang.Var       #'a-var
   clojure.lang.Namespace (find-ns 'a-namespace)})

(defn value-round-trip [v vars]
  (let [steps (->> (reductions
                     (fn [[_prev-vr prev-v] vr]
                       (if (= prev-v :oops)
                         [vr :oops]
                         [vr (try (@vr prev-v) (catch Throwable _ :oops))]))
                     ['v v]
                     vars))
        [ok ko] (split-with #(not= (second %) :oops) steps)]
    (if (empty? ko)
      (or (= (@(first vars) v)
             (-> ok last second))
          `(~'not= ~(@(first vars) v)
                   ~(-> ok last second)))
      (str (apply str (interpose " --> " (map #(-> % first get-sym) ok)))
           (str " -💥> " (-> ko ffirst get-sym))))))

(defn check-round-trip [vs-map vars]
  (let [cnt (inc (count vars))
        possibilities (->> (selections vars cnt)
                          (map dedupe)
                          distinct
                          (filter #(= (count %) cnt))
                          (sort-by (fn [vrs] (->> vrs (map #(str (.sym %)))
                                                  vec))))]
    (doseq [p possibilities
            :let [var-syms (map #(.sym %) p)]]
      (println "round-trip for:" var-syms)
      (pprint `(= (~'-> ~'v ~(-> p first .sym))
                  (~'-> ~'v ~@var-syms)))
      (print-table
        (for [[c v] vs-map]
          {:class c :result (value-round-trip v p)}))
      (newline)(newline)(newline))))

(defn check-round-trips [vs-map vars]
  (let [vars (distinct vars)
        pairs (combinations vars 2)]
    (doseq [p pairs]
      (check-round-trip vs-map p))))


;; What we initially have
(comment
  (check-round-trips test-vals [#'symbol #'name #'keyword]))

;; NOTE: 💥 means an exception was raised.

; round-trip for: (name symbol name)
; (clojure.core/= (-> v name) (-> v name symbol name))

; |                       :class |     :result |
; |------------------------------+-------------|
; |       class java.lang.String |        true |
; |   class clojure.lang.Keyword |        true |
; |    class clojure.lang.Symbol |        true |
; |       class clojure.lang.Var | v -💥> name | • fixable
; | class clojure.lang.Namespace | v -💥> name | • fixable



; round-trip for: (symbol name symbol)
; (clojure.core/= (-> v symbol) (-> v symbol name symbol))

; |                       :class |                         :result |
; |------------------------------+---------------------------------|
; |       class java.lang.String |                            true |
; |   class clojure.lang.Keyword |                            true |
; |    class clojure.lang.Symbol |                            true |
; |       class clojure.lang.Var | (not= clojure.core/a-var a-var) | • expected
; | class clojure.lang.Namespace |                   v -💥> symbol | • fixable



; round-trip for: (keyword symbol keyword)
; (clojure.core/= (-> v keyword) (-> v keyword symbol keyword))

; |                       :class |                   :result |
; |------------------------------+---------------------------|
; |       class java.lang.String |                      true |
; |   class clojure.lang.Keyword |                      true |
; |    class clojure.lang.Symbol |                      true |
; |       class clojure.lang.Var | v --> keyword -💥> symbol | • fixable
; | class clojure.lang.Namespace | v --> keyword -💥> symbol | • fixable



; round-trip for: (symbol keyword symbol)
; (clojure.core/= (-> v symbol) (-> v symbol keyword symbol))

; |                       :class |       :result |
; |------------------------------+---------------|
; |       class java.lang.String |          true |
; |   class clojure.lang.Keyword |          true |
; |    class clojure.lang.Symbol |          true |
; |       class clojure.lang.Var |          true |
; | class clojure.lang.Namespace | v -💥> symbol | • fixable



; round-trip for: (keyword name keyword)
; (clojure.core/= (-> v keyword) (-> v keyword name keyword))

; |                       :class |                 :result |
; |------------------------------+-------------------------|
; |       class java.lang.String |                    true |
; |   class clojure.lang.Keyword |                    true |
; |    class clojure.lang.Symbol |                    true |
; |       class clojure.lang.Var | v --> keyword -💥> name | • fixable
; | class clojure.lang.Namespace | v --> keyword -💥> name | • fixable



; round-trip for: (name keyword name)
; (clojure.core/= (-> v name) (-> v name keyword name))

; |                       :class |     :result |
; |------------------------------+-------------|
; |       class java.lang.String |        true |
; |   class clojure.lang.Keyword |        true |
; |    class clojure.lang.Symbol |        true |
; |       class clojure.lang.Var | v -💥> name | • fixable
; | class clojure.lang.Namespace | v -💥> name | • fixable


;; The proposal
(defn symbol
  "Returns a Symbol with the given namespace and name. Arity-1 works
  on strings, keywords, and vars."
  {:tag clojure.lang.Symbol
  :added "1.0"
  :static true}
  ([x]
   (cond
     (symbol? x)                         x
     (instance? String x)                 (clojure.lang.Symbol/intern x)
     (instance? clojure.lang.Var x)       (.toSymbol ^clojure.lang.Var x)
     (instance? clojure.lang.Keyword x)   (.sym ^clojure.lang.Keyword x)
     ;; What changed ↓
     (instance? clojure.lang.Namespace x) (.getName ^clojure.lang.Namespace x)
     :else (throw (IllegalArgumentException. "no conversion to symbol"))))
  ([ns x] (clojure.lang.Symbol/intern ns x)))

(defn keyword
  "Returns a Keyword with the given namespace and name.  Do not use :
  in the keyword strings, it will be added automatically."
  {:tag clojure.lang.Keyword
   :added "1.0"
   :static true}
  ([name]
   (cond
     (keyword? name) name
     (symbol? name) (clojure.lang.Keyword/intern ^clojure.lang.Symbol name)
     (string? name) (clojure.lang.Keyword/intern ^String name)
     ;; What changed ↓
     (instance? clojure.lang.Var name)
     (clojure.lang.Keyword/intern
       ^String (.. ^clojure.lang.Var name ns toString)
       ^String (.. ^clojure.lang.Var name sym toString))
     (instance? clojure.lang.Namespace name)
     (clojure.lang.Keyword/intern
       ^clojure.lang.Symbol (.getName ^clojure.lang.Namespace name))))
  ([ns name] (clojure.lang.Keyword/intern ns name)))

(defn name
  "Returns the name String of a string, symbol, keyword, var or namespace."
  {:tag String
   :added "1.0"
   :static true}
  [x]
  (cond
    (string? x)                          x
    (instance? clojure.lang.Named x)     (. ^clojure.lang.Named x (getName))
    ;; What changed ↓
    (instance? clojure.lang.Namespace x) (str x)
    (instance? clojure.lang.Var x)       (str (.name
                                                ^clojure.lang.Namespace
                                                (.ns ^clojure.lang.Var x))
                                              "/"
                                              (.sym ^clojure.lang.Var x))))



;; The results
(check-round-trips test-vals [#'symbol #'name #'keyword])

; round-trip for: (name symbol name)
; (clojure.core/= (-> v name) (-> v name symbol name))

; |                       :class |                             :result |
; |------------------------------+-------------------------------------|
; |       class java.lang.String |                                true |
; |   class clojure.lang.Keyword |                                true |
; |    class clojure.lang.Symbol |                                true |
; |       class clojure.lang.Var | (not= "clojure.core/a-var" "a-var") | • expected
; | class clojure.lang.Namespace |                                true | • fixed!



; round-trip for: (symbol name symbol)
; (clojure.core/= (-> v symbol) (-> v symbol name symbol))

; |                       :class |                         :result |
; |------------------------------+---------------------------------|
; |       class java.lang.String |                            true |
; |   class clojure.lang.Keyword |                            true |
; |    class clojure.lang.Symbol |                            true |
; |       class clojure.lang.Var | (not= clojure.core/a-var a-var) | • expected
; | class clojure.lang.Namespace |                            true | • fixed!



; round-trip for: (keyword symbol keyword)
; (clojure.core/= (-> v keyword) (-> v keyword symbol keyword))

; |                       :class | :result |
; |------------------------------+---------|
; |       class java.lang.String |    true |
; |   class clojure.lang.Keyword |    true |
; |    class clojure.lang.Symbol |    true |
; |       class clojure.lang.Var |    true | • fixed!
; | class clojure.lang.Namespace |    true | • fixed!



; round-trip for: (symbol keyword symbol)
; (clojure.core/= (-> v symbol) (-> v symbol keyword symbol))

; |                       :class | :result |
; |------------------------------+---------|
; |       class java.lang.String |    true |
; |   class clojure.lang.Keyword |    true |
; |    class clojure.lang.Symbol |    true |
; |       class clojure.lang.Var |    true |
; | class clojure.lang.Namespace |    true | • fixed!



; round-trip for: (keyword name keyword)
; (clojure.core/= (-> v keyword) (-> v keyword name keyword))

; |                       :class |                           :result |
; |------------------------------+-----------------------------------|
; |       class java.lang.String |                              true |
; |   class clojure.lang.Keyword |                              true |
; |    class clojure.lang.Symbol |                              true |
; |       class clojure.lang.Var | (not= :clojure.core/a-var :a-var) | • expected
; | class clojure.lang.Namespace |                              true | • fixed!



; round-trip for: (name keyword name)
; (clojure.core/= (-> v name) (-> v name keyword name))

; |                       :class |                             :result |
; |------------------------------+-------------------------------------|
; |       class java.lang.String |                                true |
; |   class clojure.lang.Keyword |                                true |
; |    class clojure.lang.Symbol |                                true |
; |       class clojure.lang.Var | (not= "clojure.core/a-var" "a-var") | • expected
; | class clojure.lang.Namespace |                                true | • fixed!
