(ns shuriken.threading
  (:use clojure.pprint
        shuriken.debug)
  (:require [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha]
            [clojure.test.check.generators :as gen]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.macro :refer [symbol-macrolet]]
            [shuriken.debug :refer [debug-print]]
            [shuriken.string :refer [adjust truncate lines format-code]]))

;; TODO: maybe debug-print could stay private

(defmacro tap->
  "Threads the expr through the forms like -> and returns the value of the
  initial expr."
  [x & forms]
  `(let [result# ~x]
     (-> result# ~@forms)
     result#))

(defmacro tap->>
  "Like tap-> but threads with ->>."
  [x & forms]
  `(let [result# ~x]
     (->> result# ~@forms)
     result#))

(defmacro tap
  "Evaluates expressions in order returning the value of the first.
  Will thread the first expr into any subsequent expr starting with a threading
  macro symbol.
  
  (tap 123
       (println \"yo\")
       (some-> inc println))
  ; yo
  ; 124
  ; => 123"
  [x & body]
  (let [result-sym (gensym "result-")]
    `(let [~result-sym ~x]
       ~@(map (fn [expr]
                (if (some->> expr first str (re-matches #"^.*->>?$"))
                  `(-> ~result-sym ~expr)
                  expr))
              body)
       ~result-sym)))

(defn pp->log* [variant max-label-length label result]
  (let []
    (debug-print
      (adjust :left max-label-length label)
      result)))

(defmacro literally [expr]
  `(let [expr# '~expr
         value# ~expr]
     [(= (str expr#) (str value#))
      value#]))

; (defmacro with-threading-variables [& body]
;   (map (fn [[param & more]]
;          `(~param
;             (symbol-macrolet [~(symbol "&thread-macro") ~specific-name]
;               (let [~(symbol "&thread-opts") ~opts]
;                 ~@more))))))

; (println "-->" (s/conform (s/cat :doc      (s/? string?)
;                                  :attr-map (s/? map?)
;                                  :params    vector?)
;                           '[[params]]))

(defn conf
  ([f form]     (s/& form (s/conformer f)))
  ([f unf form] (s/& form (s/conformer f unf))))

(defmacro either [& {:as cases}]
  `(conf #(with-meta (val %)
            {::source-either-case (key %)})
         (fn [x#]
           (let [original-case# (-> x# meta ::source-either-case)
                 default# ~(:default cases)
                 ex# (fn [incipit#]
                      (Exception.
                        (str incipit# " in 'either' statement:\n"
                             ~(with-out-str (pprint cases))
                             "unforming value\n"
                             (with-out-str (pprint x#)))))]
             (cond
               original-case#      [original-case# x#]
               (keyword? default#) [default# x#]
               (ifn? default#)     [(default# x#) x#]
               (nil? default#)     (throw (ex# "Missing default case"))
               :else               (throw (ex# "Invalid default case")))))
         (s/alt ~@(->> (dissoc cases :default)
                       (apply concat)))))

(s/def ::macro-args+body
  (s/cat :args vector?
         :body (conf #(apply list %) vec
                     (s/* any?))))

(s/def ::macro-definition
  (s/cat
    :def-macro  symbol?
    :name       symbol?
    :doc-string (s/? string?)
    :attr-map   (s/? map?)
    :bodies     (either
                  :arity-1 (conf vector first ::macro-args+body)
                  :arity-n (s/+ (s/and list? ::macro-args+body))
                  :default (fn [x]
                             (if (= (count x) 1)
                               :arity-1
                               :arity-n)))))

(s/def ::threading-macro-variants
       (s/and vector?
              (s/* (s/cat
                     :macro     symbol?
                     :docstring (s/? string?)
                     :opts      (s/? map?)))))

(s/def ::def-threading-macro-args
       (s/cat
         :name     symbol?
         :main-doc (s/? string?)
         :variants ::threading-macro-variants
         :params   vector?
         :body     (conf #(apply list %) vec
                         (s/+ any?))))

; (->> '(defmacro abc ([] 123) ([a] 456))
;      (s/conform ::macro-definition)
;      str
;      read-string
;      (s/unform ::macro-definition)
;      pprint)

(defmacro def-threading-macro [& args]
  (let [{:keys [name main-doc? variants params & body]}
        (s/conform ::def-threading-macro-args args)]
    (for [{:keys [macro docstring opts]} variants
          :let [specific-name (symbol (str name macro))
                specific-doc (str (if main-doc?
                                    (str main-doc? \newline)
                                    "")
                                  docstring)
                body (clojure.walk/postwalk
                       (fn [x]
                         (or (and (symbol? x)
                                  (namespace x)
                                  (contains? #{"&thread-macro"
                                               "&thread-opts"}
                                             (.getName x))
                                  (-> x .getName symbol))
                             x))
                       body)]]))
  #_(let [main-doc  (and (string? a) a)
          more (if main-doc more (cons a more))
          [variants & more] more 
          ;; more is now (params expr*) or ((params body*)*)
          bodies (if (-> more first vector?)
                   (list more)
                   more)]
      (s/conform )
      #_`(do ~@(for [{:keys [macro docstring opts]}
                     (s/conform ::threading-macro-variants variants)
                     :let [specific-name (symbol (str name macro))
                           specific-doc (str (if main-doc
                                               (str main-doc \newline)
                                               "")
                                             docstring)]
                     bodies (->> bodies
                                 (clojure.walk/postwalk
                                   (fn [form]
                                     (if-let [replacement (and (symbol? form)
                                                               (namespace form)
                                                               (contains? #{"&thread-macro"
                                                                            "&thread-opts"}
                                                                          (.getName form))
                                                               (-> form .getName symbol))]
                                       replacement
                                       form)))
                                 (map (fn [[param & more]]
                                        `(~param
                                           (symbol-macrolet [~(symbol "&thread-macro") ~specific-name]
                                             (let [~(symbol "&thread-opts") ~opts]
                                               ~@more))))))]             
                 `(defmacro ~specific-name ~@(remove empty? [specific-doc])
                    ~bodies)))))

(pprint
  (macroexpand-1
    '(def-threading-macro toto
      [-> 
       ->> "xyz"
       ->>>> {:a 1}]
      [a]
      `(&thread-macro a (- 100)))))

#_(def-threading-macro toto
  [-> 
   ->> "xyz"
   ->>>> {:a 1}]
  [a]
  `(&thread-macro a (- 100)))

; (pprint (macroexpand-1 '(toto-> 1)))
; (throw (Exception. "fin"))
; (println "toto->"  (toto-> 1))
; (println "toto->>" (toto->> 1))

  
  

(defmacro pp->
  "Like ->, but prints a debug statement for f and each expr in forms."
  [f & forms]
  (let [variant (symbol "->")
        max-expr-length (->> forms
                             (map #(->> % format-code lines first
                                        (str variant " ") count))
                             (apply max))
        log-sym (gensym "log")
        padding (apply str (-> variant (str " ") count (repeat " ")))]
    `(let [[literal?# result-f#] (literally ~f)
           label-f# (str ~(str variant) (when-not literal?# (str " " '~f)))
           max-label-length# (max ~max-expr-length
                                  (min (count label-f#)
                                       24))
           ~log-sym (partial pp->log*
                             ~(str variant)
                             max-label-length#)]
       (~log-sym (truncate label-f# max-label-length#)
                 result-f#)
       (-> result-f#
           ~@(map (fn [expr]
                    `(tap->> ~expr (~log-sym ~(str padding expr))))
                  forms)))))

#_(pp-> (cons 'abc '(let [a 1 b 2]
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)
                   (+ a b)))
      identity)

(defmacro pp->>
  "Like ->>, but prints a debug statement for f and each expr in forms."
  [f & forms]
  (let [max-expr-length (->> forms
                             (map (comp count str))
                             (filter #(< % 40))
                             (apply max))
        log-sym (gensym "log")]
    `(let [~log-sym (partial pp->log* ~max-expr-length)]
       (-> (tap->> ~f (~log-sym '~f "->>"))
           ~@(map (fn [expr]
                    `(-> (->> ~expr)
                         (tap->> (~log-sym '~expr ~(str "   " expr)))))
                  forms)))))

; (defmacro if->
;   ([element test then]
;    `(if-> ~element ~test ~then identity))
;   ([element test then else]
;     `(let [e# ~element]
;        (if (-> e# ~test)
;          (-> e# ~then)
;          (-> e# ~else)))))

; (defmacro if->> [element test expr]
;   `(let [e# ~element]
;      (if (->> e# ~test)
;        (->> e# ~expr)
;        e#)))

; (defmacro if->>
;   ([element test then]
;    `(if-> ~element ~test ~then identity))
;   ([element test then else]
;     `(let [e# ~element]
;        (if (~test e#)
;          (~then e#)
;          (~else e#)))))
