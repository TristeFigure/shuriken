(ns shuriken.string
  "### Functions on strings"
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint] :as pprint]
            [shuriken.sequential :refer [slice]]))

(defn format-code
  "Returns a string as formatted by `clojure.pprint/code-dispatch`."
  [code]
  (str/trim
    (with-out-str
      (binding
        [pprint/*print-right-margin*    130
         pprint/*print-miser-width*     80
         pprint/*print-pprint-dispatch* pprint/code-dispatch]
        (pprint code)))))

(defn adjust
  "Left or right-adjust a string."
  [direction n s]
  {:pre [(contains? #{:left :right} direction)]}
  (let [orientation (if (= direction :left)
                      "-"
                      "")]
    (format (str "%" orientation n "s")
            s)))

(defn lines
  "Splits a string around newlines."
  [s]
  (str/split s #"\r?\n"))

(defn words
  "Splits a string around whitespaces."
  [s]
  (->> (slice #(re-matches #"\s" (str %)) s :include-empty false)
       (map #(apply str %))))

(defn join-lines
  "Glues strings together with newlines."
  [lines]
  (str/join "\n" lines))

(defn join-words
  "Glues strings together with spaces."
  [words]
  (str/join " " words))

(defn tabulate
  "Left-pad a string with `pad`, taking newlines into account."
  [s pad]
  (->> (lines s)
       (map #(str pad %))
       join-lines))

(defn truncate
  "Truncate a string with `pad` beyond a certain length. By default,
  `pad` is `\"...\"`."
  ([s length]
   (truncate s length "..."))
  ([s length pad]
   (if (> (count s) length)
     (-> (->> s (take length) (apply str))
         (str pad))
     s)))

(defmacro no-print
  "Binds *out* to an anonymous writer used as /dev/null and returns
  the value of the last expr in body."
  [& body]
  `(binding [*out* (new java.io.StringWriter)]
     ~@body))
