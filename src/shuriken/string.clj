(ns shuriken.string
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint] :as pprint]))

;; TODO: expose and document
(defn format-code [code]
  (str/trim
    (with-out-str
      (binding
        [pprint/*print-right-margin*    130
         pprint/*print-miser-width*     80
         pprint/*print-pprint-dispatch* pprint/code-dispatch]
        (pprint code)))))

;; TODO: expose and document
(defn adjust [direction n s]
  {:pre [(contains? #{:left :right} direction)]}
  (let [orientation (if (= direction :left)
                      "-"
                      "")]
    (format (str "%" orientation n "s")
            s)))

;; TODO: expose and document
(defn lines [s]
  (str/split s #"\r?\n"))

;; TODO: expose and document
(defn join-lines [lines]
  (str/join "\n" lines))

;; TODO: document
(defn tabulate [s with]
  (->> (lines s)
       (map #(->> % (str with)))
       join-lines))

;; TODO: document
(defn truncate
  ([s length]
   (truncate s length "..."))
  ([s length with]
   (if (> (count s) length)
     (-> (->> s (take length) (apply str))
         (str with))
     s)))

;; TODO
(defn regex-quote [s]
  (java.util.regex.Pattern/quote s))
