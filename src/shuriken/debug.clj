(ns shuriken.debug
  (:require [shuriken.string
              :refer [format-code lines join-lines tabulate truncate]]
            [clojure.string :as str]))

(defn debug-print [label result]
  (let [label-length (+ 1 (count label)) ;; accomodating ":"
        result-str (format-code result)
        [f & more] (lines result-str)
        tabs (apply str (repeat label-length " "))]
    (print (str label ": " f))
    (println (tabulate (join-lines more)
                       tabs))))

(defmacro debug
  "Evaluates and prints a debug statement for each form.
  Returns the value of the last expression."
  [& body]
  (when (seq body)
    (let [v-sym (gensym "v-")
          bindings (vec (mapcat (fn [expr]
                                  `[~v-sym
                                    (let [result# ~expr
                                          label# ~(-> expr str (truncate 12))]
                                      (debug-print label# result#)
                                      result#)])
                                body))]
      `(let ~bindings
         ~v-sym))))
