(ns shuriken.reader-macros.sexp-macros
  (:use clojure.pprint shuriken.macro)
  (:refer-clojure :exclude [read])
  (:require [reader-macros.core :refer [lisp-readers set-macro-character]]
            [clojure.spec.alpha :as s]
            [shuriken.exception :refer [silence]]
            [shuriken.reflection :refer [static-method]]
            [shuriken.spec :refer [conform!]]
            [shuriken.monkey-patch :refer [copy-class only]])
  (:import [clojure.lang LispReader LispReader$Resolver]
           [java.io Reader PushbackReader IOException]
           [java.nio CharBuffer]))

(def ^:private lparen (first "("))
(def ^:private rparen (first ")"))

(def ^:private read1
  (static-method LispReader 'read1 [Reader]))

(def ^:private unread
  (static-method LispReader 'unread [PushbackReader Integer/TYPE]))

(def ^:private read-delimited-list
  (static-method LispReader 'readDelimitedList
                 [Character/TYPE PushbackReader Boolean/TYPE Object Object]))

(def ^:private whitespace?
  (comp boolean ;; In Clojure Booleans (boxing) are treated as truthy values wtf
        (static-method LispReader 'isWhitespace [Integer/TYPE])
        int))

(def ^:private get-macro
  (static-method LispReader 'getMacro [Integer/TYPE]))

(def ^:private interpret-token
  (static-method LispReader 'interpretToken [String LispReader$Resolver]))


(definterface IDynamicPushbackReader
  (^void unreadString [^String s]))

(defmacro ^:private guard-not-closed! []
  `(when (deref ~'closed) (throw (IOException. "Closed"))))

(defn- unsupported []
  (throw (IOException. "Unsupported")))

(defn- aset-chars
  ([src dst] (aset-chars src dst 0))
  ([src dst off]
   (doseq [[i v] (map-indexed vector src)]
     (aset-char dst (+ off i) v))
   (count src)))

(defn dynamic-pushback-reader [^Reader r]
  (let [pushback-buffer (atom '())
        closed          (atom false)]
    (proxy [PushbackReader IDynamicPushbackReader] [r]
      (close []                 (.close r)
                                (reset! closed true))
      (mark [_read-ahead-limit] (unsupported))
      (markSupported []         false)
      (read ([]                 (guard-not-closed!)
                                (if-let [c (first @pushback-buffer)]
                                  (do (swap! pushback-buffer rest)
                                      (int c))
                                  (.read r)))
            ;; char[] or CharBuffer
            ([chs]              (guard-not-closed!)
                                (if (instance? CharBuffer chs)
                                  (let [len    (.length chs)
                                        ar     (char-array len)
                                        n-read (.read this ar 0 len)]
                                    (.put chs ar)
                                    n-read)
                                  (.read this chs 0 (count chs))))
            ;; char[]
            ([chs off len]      (guard-not-closed!)
                                (let [;; deal with the buffer first
                                      [from-buf new-buf] (split-at
                                                           len @pushback-buffer)
                                      buf-read-len (count from-buf)
                                      rem-len  (- len buf-read-len)
                                      _ (aset-chars from-buf chs)
                                      _ (reset! pushback-buffer new-buf)
                                      ;; then deal with the reader
                                      r-len (if (> rem-len 0)
                                              (let [newoff (+ off buf-read-len)]
                                                (.read r chs newoff rem-len))
                                              0)]
                                  (if (= r-len -1)
                                    -1
                                    (+ buf-read-len r-len)))))
      (ready []                 (guard-not-closed!)
                                (or (not (empty? @pushback-buffer))
                                    (.ready r)))
      (reset []                 (unsupported))
      (skip [n]                 (guard-not-closed!)
                                (let [buf     @pushback-buffer
                                      buf-len (count buf)
                                      diff    (- n buf-len)]
                                  (if (<= diff 0)
                                    (do (swap! pushback-buffer (partial drop n))
                                        n)
                                    (do (reset! pushback-buffer '())
                                        (+ buf-len
                                           (.skip r diff))))))
      (unread ;; integer or char[]
              ([x]              (guard-not-closed!)
                                (if (int? x) ;; int/long/short/byte (fixed-prec)
                                  (swap! pushback-buffer #(cons (char x) %))
                                  (swap! pushback-buffer #(concat x %)))
                                nil)
              ;; char[]
              ([chs off len]    (guard-not-closed!)
                                (let [sub-chs (subvec
                                                (into [] chs) off (+ off len))]
                                  (swap! pushback-buffer #(concat sub-chs %))
                                  nil)))

      (unreadString [s]         (swap! pushback-buffer #(concat s %))
                                nil))))

(defn unread-string [r s]
  (let [dpr (if (instance? IDynamicPushbackReader r)
              r (dynamic-pushback-reader r))]
    (.unreadString dpr s)
    dpr))

(def ^:shuriken/sexpr-reader-macro def-sexpr-reader-macro
  (fn def-sexpr-reader-macro [&form & _args]
    (let [m (conform! :shuriken.spec/defn-form &form)]
      (s/unform
        :shuriken.spec/defn-form
        (-> (assoc m :op 'clojure.core/defn)
            (update :bodies
                    (fn [bodies]
                      (mapv #(update % :args
                                     (comp vec (partial cons '&form)))
                            bodies)))
            (update :name vary-meta assoc :shuriken/sexpr-reader-macro true))))))

(def ^:shuriken/sexpr-reader-macro def-sexpr-reader-macro-raw
  (fn def-sexpr-reader-macro-raw [&form & _args]
    (let [m (conform! :shuriken.spec/defn-form &form)]
      (s/unform
        :shuriken.spec/defn-form
        (-> (assoc m :op 'clojure.core/defn)
            (update :bodies
                    (fn [bodies]
                      (assert (some #(let [args (-> % :args count)]
                                       (or (>= (count args) 3)
                                           (.contains args '&)))
                                    bodies)
                              (str "Raw sexp read macros must have arity >= 3 "
                                   "in order to accept as args 'reader', "
                                   "'opts' and 'pending-forms'."))
                      (mapv #(update % :args (comp vec (partial cons '&form)))
                            bodies)))
            (update :name vary-meta assoc :shuriken/sexpr-reader-macro :raw))))))

(defn sexpr-reader-macro? [a-var]
  (-> a-var meta :shuriken/sexpr-reader-macro))

(defn raw-sexpr-reader-macro? [a-var]
  (-> a-var meta :shuriken/sexpr-reader-macro (= :raw)))

(defn perceive-list [reader]
  (->> (repeatedly #(read1 reader))
       (reductions (fn [[cnt acc] c]
                     (let [ch (char c)]
                       [(condp = ch
                          lparen (inc cnt)
                          rparen (dec cnt)
                          cnt)
                        (str acc ch)]))
                   [1 ""])
       (take-while #(-> % first (> 0)))
       last second))

(defn read-while
  "Consumes chars from `reader` one at a time while `pred` matches
  the character being read and -1/EOF has not been read, returning
  them as a String in the end.

  The character that did not match `pred` will be unread."
  [pred ^PushbackReader reader]
  (let [sb (StringBuilder.)]
    (->> (repeatedly #(char (read1 reader)))
         (take-while #(and (not= -1 %)
                           (or (pred %)             ;; unread unmatching char
                               (unread reader %)))) ;; unread always returns nil
         (run! #(.append sb %)))
    (when-not (zero? (.length sb))
      (.toString sb))))

(defn eval-sexpr-reader-macro [reader paren opts pending-forms v]
  (if (-> v meta :shuriken/sexpr-reader-macro (= :raw))
    (@v reader)
    (let [lst (original-list-reader reader paren opts pending-forms)]
      (apply @v lst (rest lst)))))

(defonce original-list-reader
  (do (println "MARABOUT")
      (get (lisp-readers) lparen)))

(defn- read-list-head [reader]
  (read-while #(and (not (whitespace? %))
                    (not (get-macro %)))
              reader))

(defn list-reader-with-sexpr-macros [reader paren opts pending-forms]
  (let [head       (read-list-head reader)
        rewinded-r (unread-string reader head)
        as-usual   #(original-list-reader rewinded-r paren opts pending-forms)]
    (if-not head
      (as-usual)
      (let [tok (interpret-token head *reader-resolver*)
            tok-var (and (symbol? tok)
                         (silence ClassNotFoundException
                           (resolve tok)))]
        (if (and tok-var (sexpr-reader-macro? tok-var))
          (eval-sexpr-reader-macro rewinded-r paren opts pending-forms tok-var)
          (as-usual))))))

(set-macro-character lparen list-reader-with-sexpr-macros)

(def-sexpr-reader-macro doit  [a b c]
  (println "[a b c]:" [a b c])
  (println "classes:" (map class [a b c]))
  (println "*ns*:" *ns*)
  nil)

(doit (+ 1 1) 2 3)
