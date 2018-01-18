(ns shuriken.threading
  (:use clojure.pprint)
  (:require [clojure.spec.alpha :as s]
            [shuriken.spec :refer [conf either conform!]]
            [clojure.string :as str]
            [shuriken.debug :refer [debug-print]]
            [shuriken.string :refer [adjust truncate lines format-code]]))

(s/def ::macro-variants
  (s/and vector?
         (s/* (s/cat
                :macro     symbol?
                :docstring (s/? string?)
                :opts      (s/? map?)))))

(s/def ::def-macro-variants-args
  (s/cat
    :name-prefix (s/? symbol?)
    :doc-prefix  (s/? string?)
    :variants    ::macro-variants
    :bodies      :shuriken.spec/args+bodies))

(declare &macro-variant &macro-opts)

;; TODO: document
(defmacro def-macro-variants [& args]
  (let [{:keys [name-prefix doc-prefix variants args bodies]}
        (conform! ::def-macro-variants-args args)]
    (cons 'do (for [{:keys [macro docstring opts]} variants
                    :let [specific-name (symbol (str name-prefix macro))
                          specific-doc (if (or doc-prefix docstring)
                                         (clojure.string/trim
                                           (str doc-prefix \newline docstring)))
                          bodies (map (fn [{:keys [body] :as plan}]
                                        (->> body)
                                        (assoc plan :body
                                          `((let [~'&macro-variant '~macro
                                                  ~'&macro-opts '~opts]
                                              ~@body))))
                                      bodies)]]
                (do (->> {:def-macro  'defmacro
                          :name       specific-name
                          :doc-string specific-doc
                          :attr-map   nil ; TODO
                          :bodies     bodies}
                         (remove (fn [[k v]] (nil? v)))
                         (into {})
                         (s/unform :shuriken.spec/macro-definition)))))))

(def-macro-variants tap
  "Threads the expr through the forms like -> and returns the value of
  the initial expr."
  [-> 
   ->> "Like tap-> but threads with ->>."]
  [x & forms]
  `(let [result# ~x]
     (~&macro-variant result# ~@forms)
     result#))

(defmacro tap
  "Evaluates expressions in order returning the value of the first.
  Will thread the first expr into any subsequent expr starting with a
  threading macro symbol.
  
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

(defmacro literally [expr]
  `(let [expr# '~expr
         value# ~expr]
     [(= (str expr#) (str value#))
      value#]))

(defn pp->log* [variant max-label-length label result]
  (let []
    (debug-print
      (adjust :left max-label-length label)
      result)))

(def-macro-variants pp
  [->  "Like ->, but prints a debug statement for f and each expr in forms."
   ->> "Like ->>, but prints a debug statement for f and each expr in forms."]
  [f & forms]
  (let [max-expr-length (->> forms
                             (map #(->> % format-code lines first
                                        (str &macro-variant " ") count))
                             (apply max))
        log-sym (gensym "log")
        padding (apply str (-> &macro-variant (str " ") count (repeat " ")))]
    `(let [[literal?# result-f#] (literally ~f)
           label-f# (str ~(str &macro-variant)
                         (when-not literal?# (str " " '~f)))
           max-label-length# (max ~max-expr-length
                                  (min (count label-f#)
                                       24))
           ~log-sym (partial pp->log*
                             ~(str &macro-variant)
                             max-label-length#)]
       (~log-sym (truncate label-f# max-label-length#)
                 result-f#)
       (~&macro-variant result-f#
                        ~@(map (fn [expr]
                                 `((fn [x#]
                                     (let [result# (~&macro-variant x# ~expr)]
                                       (~log-sym ~(str padding expr) result#)
                                       result#))))
                               forms)))))

(def-macro-variants if
  "Threads value through test then else. If else is not provided,
  returns the initial value when test fails."
  [-> 
   ->> "Like if->, but with ->> semantics."]
  ([element test then]
   `(~(symbol (str 'if &macro-variant))
     ~element ~test ~then identity))
  ([element test then else]
   `(let [e# ~element]
      (if (~&macro-variant e# ~test)
        (~&macro-variant e# ~then)
        (~&macro-variant e# ~else)))))
