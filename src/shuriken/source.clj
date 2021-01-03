(ns shuriken.source
  (:require [clojure.java.io :refer [file writer]]
            [clojure.pprint  :refer [pprint]]
            [clojure.repl]))

;; TODO: finsish

(def ^:private root-resource  @#'clojure.core/root-resource)
(def ^:private source-fn      @#'clojure.repl/source-fn)

(defn- pretty [x]
  (str (with-out-str (pprint x))
       \newline))

(defn- fresh-file [ns]
  (doto (file "src" (str (subs  (root-resource (.name ns))  1)
                         ".clj"))
        .delete
        (-> .getParentFile .mkdirs)
        .createNewFile))

(defn inline
  "Copies source code from the specified namespace into
  `< project-dir >/src/< imported-lib >/< imported-namespace >.clj`.
  You can limit it to a few vars with the `:only` option.
  A `ns` form will be generated at the top of the output file. Use
  the `:ns-opts` options to pass additional forms to it
  (e.g `'((:import ...))`).
  You're ultimately responsible for handling dependencies."
  [ns & {:keys [only ns-opts] :or {only any?}}]
  (let [ns (doto (the-ns ns)
                 (-> .name require))
        only (condp #(%1 %2) only
               sequential?  (set (map #(if (var? %)  %  (resolve %))
                                      only))
               fn?          only
               :else        (set [(if (var? only)  only  (resolve only))]))
        syms (->> (ns-interns ns)
                  vals
                  (filter only)
                  (sort-by #(-> % meta ((juxt :line :column))))
                  (map #(symbol (str (.name (.ns %)))
                                (str (.sym %)))))]
    (pprint syms)
    (with-open [out (writer (fresh-file ns))]
      (.write out (pretty `(ns ~(.name ns) ~@ns-opts)))
      (doseq [s syms]
        (println s)
        (pprint (source-fn s))
        (newline)
        (.write out (str (source-fn s) \newline))))))
