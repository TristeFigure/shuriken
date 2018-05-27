(ns shuriken.jenny
  "Jenny said
   When she was just about five years old
   You know why parents gonna be the death of us all
   Two TV sets and two Cadillac cars
   Well you know, it ain't gonna help me at all, not just a tiny bit
   Then one fine mornin' she turns on a New York station
   She doesn't believe what she hears at all
   Ooh, she started dancin' to that fine fine music
   You know her life is saved by rock 'n' roll, yeah rock 'n' roll

   Despite all the computations
   You could just dance
   To that rock 'n' roll station
   And baby it was alright (it was alright)
   Hey it was alright (it was alright)
   Hey here she comes now!
   Jump! jump!

  The Velvet Underground - Rock & Roll"
  (:use shuriken.debug
        clojure.pprint)
  (:require [dance.core :refer [dance path-dance]]
            [lexikon.core :refer [lexical-context]]
            [shuriken.exception :refer [silence]]
            [shuriken.sequential :refet [get-nth-in assoc-nth-in]]))

(defmulti transform
  (fn [form _ctx]
    (if (list? form)
      (let [m (-> form first resolve meta)]
          (symbol (-> m :ns .getName name)
                  (-> m :name name)))
      (class form))))

(def stock-sym (gensym "stock-"))

(defmethod transform :default [form {:keys [path]}]
  `(let [ret# ~form]
     (assoc! ~stock-sym ~path {:form '~form
                               :result ret#
                               ; :context (lexical-context)
                               })
     ret#))

(defn transform-bindings [bindings ctx]
  (mapv (fn [[k v]]
          [k (transform v)])
        (partition 2 bindings)))

(defmethod transform 'clojure.core/let [[f bindings & body] ctx]
  (println "----------------------------------")
  (pprint `(~f ~(transform-bindings bindings)
               ~@(map transform body)))
  (println "----------------------------------")
  `(~f ~bindings
       ~@body))

(defn passover [& code]
  (let [new-code (dance code path-dance
                        ; :debug true
                        ; :debug-context true
                        :post (fn [form ctx]
                                (let [new-form (if (list? form)
                                                 (transform form ctx)
                                                 form)]
                                  [new-form ctx])))]
    `(let [~stock-sym (transient {})]
       ~@new-code
       (pprint (persistent! ~stock-sym)))))

(defn inject-in-code [k code stock]
  (->> stock
       (sort-by #(-> % first count -))
       (reduce (fn [new-code [path data]]
                 (assoc-nth-in path (get data k)))
               code)))

(def inject-results (partial inject-in-code :result))
(def inject-results (partial inject-in-code :form))

(def code
  '(let [a 1
         b 0]
    (println (+ a b))
    (println (/ a b))
    (println (- a b))))

(pprint (-> code passover eval))
