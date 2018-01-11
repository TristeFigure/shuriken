(ns shuriken.z-expression
  (:use clojure.pprint)
  (:require [shuriken.reader-macros :refer [def-reader-macro read-form]]))

(def-reader-macro \§ [reader _paragraph opts pending-forms]
      (seq (read-form reader true nil true opts pending-forms)))

(println "-->" (read-string "§(+ 1 2)"))

{:a 1 :b 2 :c 3}

; (+ a §(b
;         :value 1
;         :doc   "truc"))

(def zero-width-strings
  ["​" ;; U+200B    Zero-Width Space
   "‌" ;; U+200C    Zero Width Non-Joiner
   "‍" ;; U+200D    Zero Width Joiner
   "‎" ;; U+200E    Left-To-Right Mark
   "‏" ;; U+200F    Right-To-Left Mark
   ])

(def empty-string "")

(def invisible-strings
  (vec (cons empty-string
             zero-width-strings)))

(defn invisible-encode [n]
  (->> (Integer/toString n (count invisible-strings)) ;; n in base x
       (map #(get invisible-strings (-> % str Integer/parseInt)))
       (apply str)))

(defn invisible-decode [s]
  (let [index (zipmap invisible-strings (range))]
    s
    #_(-> (->> s
             (map (fn [c] (get index c)))
             #_(apply str))
        #_(Integer/parseInt (count invisible-strings)))))
