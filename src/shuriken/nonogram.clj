(ns shuriken.nonogram
  (:use clojure.pprint)
  (:require [clojure.set :refer :all]
            [threading.core :refer :all]
            [weaving.core :refer :all]
            [lexikon.core :refer [lexical-context]]
            [shuriken.sequential :refer [assoc-nth assoc-nth-in]]
            [shuriken.solver :refer [solve a*]]
            [net.cgrand.seqexp :as se]))

(def ^:dynamic *debug* true)

(def ^:dynamic *tab-depth* -1)

(def original-println println)

(defn println-tabs [& args]
  (original-println (apply str (repeat *tab-depth* "  ") (interpose " " args))))

(defmacro with-tabs [& body]
  `(binding [*tab-depth* (inc *tab-depth*)]
     (with-redefs [println println-tabs]
       ~@body)))

(defn debug [title value]
  (when *debug*
    (println title)
    (pprint value)
    (newline))
  value)

;; TODO: in weaving, conflate partialization and threading, i.e. (|) and ->|
;; are the same, ditto for || and ->>|

(declare gen-line-hypos)
(def x  :x)
(def o  :o)
(def xo #{:o :x})

(defn transpose [xs]
  (apply map vector xs))

(defn edit-dim [dim f]
  (for [l dim]    ;; for each lines
    (for [h l]   ;; for each hypothesis
      (for [t h] ;; for each tile
        (f t)))))

(defn as-sets [ls]
  (edit-dim ls #(set (if-not (coll? %) [%] %))))

(defn out-of-sets [ls]
  (edit-dim ls #(if (= 1 (count %))
                  (first %)
                  %)))

(defn prettify [dim]
  (edit-dim dim
            #(case %
               (:x #{:x}) '██;'■
               (:o #{:o}) (symbol "  ");'□
               #{:x :o}   '▄▄; ◩
               #{}        (symbol (str "␗ "));'⧄
               )))

(defn filter-dimension [dim-hypos orthog-hypos]
  (let [flat-dim (map (fn [lhs]
                          (apply map union lhs))
                        dim-hypos)
        flat-orthog  (map (fn [chs]
                          (apply map union chs))
                        orthog-hypos)]
    (map (fn [l1 l2]
           (let [inter (map intersection l1 l2)]
             [(if (empty? inter) l1 inter)]))
         flat-dim
         (transpose flat-orthog))))

(defn- until-fixed-point [f init]
  (->> (iterate f init)
       (partition 2 1)
       (take-while (fn [[a b]] (not= a b)))
       last last))

(defn cross-filter [nono]
  (let [{:keys [lines columns]} (:hypos nono)
        [ls cs] (until-fixed-point
                  (fn [[ls cs]]
                    (let [filtered-ls (filter-dimension ls cs)
                          filtered-cs (filter-dimension cs filtered-ls)]
                      [filtered-ls filtered-cs]))
                  [lines columns])]
    (assoc nono :lines ls :columns cs)))

(defn tile-solved? [tile]
  (= (count tile) 1))

(defn hypo-solved? [hypo]
  (every? tile-solved? hypo))

(defn line-solved? [line]
  (and (-> line count (= 1))
       (-> line first hypo-solved?)))

(defn dim-solved? [dim]
   (every? line-solved? dim))

(defn solved? [nono]
  (and (-> nono :hypos :lines   dim-solved?)
       (-> nono :hypos :columns dim-solved?)))

(defn check-solution [ls cs]
  (assert (= (apply concat ls)
             (transpose (apply concat cs)))
          "Bad solution"))

(defn gen-dim-hypos [ls cs]
  (-> (map (|| gen-line-hypos (count cs)) ls)
      as-sets))

(defn self-filter [nono]
  (let [{ls :lines cs :columns} (:hypos nono)
        dim (if (dim-solved? ls) cs ls)]
    (for [line dim]
      (let [flat-line (apply map intersection line)]))))

(defn propagate [nono]
  (-> nono
      cross-filter))

(defn tile-valid? [tile]  (seq tile))
(defn hypo-valid? [hypo]  (and (every? tile-valid? hypo)))
(defn line-valid? [line]  (every? hypo-valid? line))
(defn dim-valid? [dim]    (every? line-valid? dim))
(defn valid? [nono]       (and (-> nono :hypos :lines   dim-valid?)
                               (-> nono :hypos :columns dim-valid?)))

(defn count-tile-choices [tile] (count tile))
(defn count-hypo-choices [hypo] (reduce + (map count-tile-choices hypo)))
(defn count-line-choices [line] (reduce + (map count-hypo-choices line)))
(defn count-dim-choices [dim]   (reduce + (map count-line-choices dim)))
(defn count-choices [nono]      (-> nono :hypos
                                    (•- (<- (+ (-• :lines count-dim-choices)
                                               (-• :columns count-dim-choices))))))

(defn matches-hint? [hypo hint]
  (-> (map (| se/repeat (| contains? x))
           hint)
      (->> (interpose (se/+ (| contains? o))))
      (as-> $
        (let [optional-o [(se/* (| contains? o))]]
          (se/exec (apply se/cat (concat optional-o $ optional-o))
                   hypo)))))

(defn gen-line-hypos*
  ([size hint] (gen-line-hypos* size hint 0))
  ([size hint offset]
   (let [[h & more-hint] hint]
     (apply concat
            (for [off (range offset size)]
              (let [off-vec       (repeat off o)
                    hint-vec      (repeat h x)
                    beginning-vec (concat off-vec hint-vec)]
                (if (empty? more-hint)
                  [(concat beginning-vec
                           (repeat (- size (count beginning-vec))
                                   o))]
                  (let [end-vecs (gen-line-hypos* (- size h off) more-hint 1)]
                    (map (fn [end-vec]
                           (concat beginning-vec end-vec))
                         end-vecs)))))))))

(defn gen-line-hypos [size hint]
  (if (empty? hint)
    [(repeat size o)]
    (vec (gen-line-hypos* size hint 0))))

(defn init [nono]
  (let [ls (-> nono :hints :lines)
        cs (-> nono :hints :columns)
        line-hypos (gen-dim-hypos ls cs)
        col-hypos  (gen-dim-hypos cs ls)]
    (assoc nono :hypos {:lines   line-hypos
                        :columns col-hypos})))

(defn tile-choices [state]
  (let [ls (-> state :hypos :lines)
        cs (-> state :hypos :columns)
        [dim-kw dim] (if (dim-solved? ls)
                       [:columns cs]
                       [:lines   ls])]
    (for [[i line]  (map-indexed vector dim)
          :when     (not (line-solved? line))
          [j hypo]  (map-indexed vector line)
          :when     (not (hypo-solved? hypo))
          [k tile]  (map-indexed vector hypo)
          :when     (not (tile-solved? tile))
          v         tile
          :let  [new-hypo (assoc-nth hypo k #{v})]
          :when (matches-hint?
                  new-hypo (-> state :hints dim-kw (nth i)))]
      [[[dim-kw i j k] #{v}]
       (-> state
           (assoc-nth-in [:hypos dim-kw i j] new-hypo)
           #_propagate)])))

(defn hypo-choices [state]
  (let [ls (-> state :hypos :lines)
        cs (-> state :hypos :columns)
        [dim-kw dim orthog-kw orthog] (if true #_(dim-solved? ls)
                                        [:columns cs :lines   ls]
                                        [:lines   ls :columns cs])]
    (for [[i line]  (map-indexed vector dim)
          :when     (not (line-solved? line))
          hypo      line
          :let [new-line (list hypo)
                new-orthog
                (for [[j col] (map-indexed vector orthog)
                      :let [new-col
                            (for [col-hypo col
                                  :when (seq (intersection (nth col-hypo i)
                                                           (nth hypo     j)))]
                              col-hypo)]]
                  new-col)]
          :when (and (seq new-orthog)
                     (every? seq new-orthog))]
      [[[dim-kw i] new-line]
       (-> state
           (assoc-nth-in [:hypos dim-kw i] new-line)
           (assoc-in [:hypos orthog-kw] new-orthog))])))

(defn print-tiles [dim]
  (doseq [l (prettify dim)]
    (doseq [[i t] (map-indexed vector (first l))]
      (print (str (when (zero? i) "   ") t)))
    (print "\n")))

(defn print-solution [result]
  (println "-- SOLUTION --")
  ; (println " - lines")
  (print-tiles (-> result :state :hypos :lines))
  (dotimes [_ 3] (newline))

  ; (println " - columns")
  ; (print-tiles (-> result :state :hypos :columns))
  ; (newline)
  )

(defn show-progression| [nono freq]
  (let [initial-cost (count-choices (init nono))
        min-choices (* (-> nono :hints :lines   count)
                       (-> nono :hints :columns count)
                       2)
        counter (atom 0)]
    (fn [the-nono]
      (let [cost (count-choices the-nono)
            progress (int (* 100 (- 1 (/ (- cost min-choices)
                                         (- initial-cost min-choices)))))]
        (when (mod @counter freq)
              (binding [*flush-on-newline* false]
                (print (str "\r- Progress: " progress "%")))
              (when (solved? the-nono)
                (newline))))
      (swap! counter inc)
      the-nono)))

(defn show-state [nono]
  (pprint (-> nono
              (update-in [:hypos :lines]   out-of-sets)
              (update-in [:hypos :columns] out-of-sets)))
  (newline)
  nono)

(defn nonogram [nono]
  {:init  (-> nono init #_propagate)
   :goal? solved?
   :actions :not-needed
   :transitions (fn [_actions state]
                  (when (valid? state)
                    (map (fn [[path state]]
                           [path (propagate state)])
                         (hypo-choices state))))})

(defn solve-nonogram [nono]
  (solve
    (nonogram nono)
    (a* (->| #_(show-progression| nono 1000)
             show-state
             count-choices))))


(def dromadaire
  {:hints {:lines   [[1 2] [2] [1] [1] [2] [2 4] [2 6] [8] [1 1] [2 2]]
           :columns [[2] [3] [1] [2 1] [5] [4] [1 4 1] [1 5] [2 2] [2 1]]}})

(def nono
  {:hints {:lines   [[] [] [3 1] [] []]
           :columns [[1] [1] [1] [] [1]]}})

(def bug-nono
  {:hints {:lines   [[] [] [1] [] []]
           :columns [[] [] [1] [] []]}})

(def smiley
  {:hints {:lines   [[6] [1 1] [1 1] [1 2 2 1] [1 1] [1 2 1] [1 1 1 1] [1 4 1] [1 1] [6]]
           :columns [[6] [1 1] [1 1 1 1] [1 1 1 1] [1 1 1 1] [1 1 1 1] [1 1 1 1] [1 1 1 1] [1 1] [6]]}})

(def swan
  {:hints {:lines   [[2 1] [1 3] [1 2] [3] [4] [1]]
           :columns [[1] [5] [2] [5] [2 1] [2]]}})

(def duck
  {:hints {:lines   [[3] [2 1] [3 2] [2 2] [6] [1 5] [6] [1] [2]]
           :columns [[1 2] [3 1] [1 5] [7 1] [5] [3] [4] [3]]}})



; (print-solution (solve-nonogram swan))
; (dotimes [_ 3] (newline))

(print-solution (solve-nonogram swan))

; (time (print-solution (solve-nonogram nono)))
; (time (print-solution (solve-nonogram bug-nono)))
; (time (print-solution (solve-nonogram swan)))
; (time (print-solution (solve-nonogram duck)))
