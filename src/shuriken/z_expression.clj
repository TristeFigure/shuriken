(ns shuriken.z-expression
  (:use clojure.pprint)
  (:require [shuriken.reader-macros :refer [def-reader-macro read-form]]
            [shuriken.dance :refer [dance merge-dances leaf-collecting-dance]]]
            [clojure.data :refer [diff]]))

(def-reader-macro \§ [reader _paragraph opts pending-forms]
      (seq (read-form reader true nil true opts pending-forms)))

; (println "-->" (read-string "§(+ 1 2)"))

{:a 1 :b 2 :c 3}

(defn parse [s]
  (loop [s s stack '() acc []]
    (let [[c & cs] s]
      (case
        (\( \[ \{) (recur cs (cons c stack) acc)
        (\) \] \}) (let [e (peek stack)]
                     (assert (= c ({\( \) \[ \] \{ \}} e)))
                     (recur cs (pop stack)))
        (let [ccs (split-with (complement ))]
          (recur cs stack (take-)))))))

(println "-->" (parse "(+ 1 (inc 2))"))

; (+ a §(b
;         :value 1
;         :doc   "truc"))

; (def zero-width-strings
;   ["​" ;; U+200B    Zero-Width Space
;    "‌" ;; U+200C    Zero Width Non-Joiner
;    "‍" ;; U+200D    Zero Width Joiner
;    "‎" ;; U+200E    Left-To-Right Mark
;    "‏" ;; U+200F    Right-To-Left Mark
;    ])

; (def empty-string "")

; (def invisible-strings
;   (vec (cons empty-string
;              zero-width-strings)))

; (defn invisible-encode [n]
;   (->> (Integer/toString n (count invisible-strings)) ;; n in base x
;        (map #(get invisible-strings (-> % str Integer/parseInt)))
;        (apply str)))

; (defn invisible-decode [s]
;   (let [index (zipmap invisible-strings (range))]
;     s
;     #_(-> (->> s
;              (map (fn [c] (get index c)))
;              #_(apply str))
;         #_(Integer/parseInt (count invisible-strings)))))

; (def v1 '(let [a 1 b 2 c 3] [a b c]))

; (def v2 '(let [a 1 b 2 c 4] [a b c]))

; (let [[d dd _] (diff v1 v2)]
;   (println "-->" (dance dd
;                    leaf-collecting-dance
;                    :pre? (not| nil?))))
