(ns shuriken.solver
  (:use clojure.pprint
        clojure.data)
  (:require [clojure.data.priority-map :refer [priority-map-by  ]]))

;; TODO: integrate in shuriken.core

; (`    |     _
; _) () | \/ (/_ |`

(defn solve
  "Solves `problem` using `strategy`.
  - problem is a map with keys
  ```
  :init           • the initial state
  :goal?          • a fn taking a state that decides
                    whether the goal has been reached
  :actions        • a coll of  (distinct) actions
  :transitions    • a fn taking the actions and a state
                    that returns a set of pairs [a s]
                    where : - a  is the action that caused s
                            - s  is the next state

  - strategy is a map with keys
  :create-fringe  • a fn taking the initial search node
                    that returns the strategy fringe
  :select-node    • fn taking the fringe and returning
                    a coll [n f]
                    where : - n  is a node
                            - f  is the fringe without n
  :add-nodes      • fn taking the fringe and a seq of
                    nodes that returns the new fringe
  ```"
  [problem strategy]
  (let [{:keys [init goal? actions transitions]} problem
        {:keys [create-fringe select-node add-nodes]} strategy]
    (loop [fringe (create-fringe {:path [] :cause nil :state init})]
      (let [[node new-fringe] (select-node fringe)
            {:keys [path cause state]} node
            trans (transitions actions state)
            next-nodes (map (fn build-node [[a s]]
                              {:path (conj path [cause state])
                               :cause a :state s})
                            trans)]
        (if (goal? (:state node))
          node
          (recur (add-nodes new-fringe next-nodes)))))))


; (` |-       |-  _     .  _   _
; _) |_ |` (| |_ (/_ (| | (/_ _\
;                    _|                                     `-'

(def dfs
  "Depth-first search strategy."
  {:create-fringe list
   :select-node   (juxt peek pop)
   :add-nodes     into})

(def bfs
  "Breadth-first search strategy"
  {:create-fringe #(conj clojure.lang.PersistentQueue/EMPTY %)
   :select-node (juxt peek pop)
   :add-nodes (partial reduce conj)})

(defn a*
  "A-star search strategy. `cost-fn` is passed a node's state."
  [cost-fn]
  {:create-fringe (fn [node]
                    (priority-map-by
                      (fn [a b]
                        (apply compare (map #(-> % :state cost-fn) [a b])))
                      (:state node)
                      node))
   :select-node #(vector (-> % first val) (dissoc % (-> % first key)))
   :add-nodes (partial reduce #(assoc %1 (:state %2) %2))})

; |\ |    ,_  |   _       ,_     _   _
; | \| L| ||| |) (/_ |`   ||| (| /_ (/_

;; For an initial number n find the series of steps leading to a final
;; number m, where each step can be obtained by applying to the result
;; of the previous step x one of these operations:
;;   - x + 2
;;   - x * 2
;;   - x / 2 (if x is divisible by 2)

(defn number-maze [start target]
  {:init start
   :goal? #{target}
   :actions {:double #(* 2 %)
             :halve #(when (even? %) (/ % 2))
             :plus-two #(+ 2 %)}
   :transitions (fn transit [actions state]
                  (->> (for [[name func] actions]
                         (when-let [next-state (func state)]
                           [name next-state]))
                       (remove nil?)))})

; (let [initial 9
;       target  1592]
;   (-> (solve (number-maze initial target)
;              #_bfs                          ;; 6396.461 msecs optimal
;              (a* #(Math/abs (- target %)))) ;; 1514.043 msecs horrible
;       pprint))


; |\/| .  _  _ .    ,_       .  _   _   ()    _    ,_ ,_ . |     |  _
; |  | | _\ _\ | () || (| |` | (/_ _\   (X   (_ (| || || | |) (| | _\

;; Three missionaries and three cannibals must cross a river using a
;; boat which can carry at most two people, under the constraint
;; that, for both banks, if there are missionaries present on the
;; bank, they cannot be outnumbered by cannibals (if they were, the
;; cannibals would eat the missionaries). The boat cannot cross the
;;  river by itself with no people on board.
(defn between [x n m]
  (and (>= x n) (<= x m)))

(defn cannibalism? [[[lm lc] [rm rc] boat-position]]
  (let [x (or (and (not (= 0 lm))
                   (not (= 0 lc))
                   (< lm lc))
              (and (not (= 0 rm))
                   (not (= 0 rc))
                   (< rm rc)))]
    x))

(defn legal-state? [[left-bank right-bank boat-position]]
  (and (every? #(between % 0 3) (concat left-bank right-bank))
       (every? #{3} (map (comp (partial reduce +) vector)
                         left-bank right-bank))))

(def missionaries-and-cannibals
  {:init [[3 3] [0 0] :left]
   :goal? #{[[0 0] [3 3] :right]}
   :actions [[1 0] [2 0] [1 1] [0 2] [0 1]]
   :transitions
   (memoize
     (fn mc-transit [actions [l r p]]
       (->> actions
            (map (if (= p :left)
                   #(vector % [(map - l %) (map + r %) :right])
                   #(vector % [(map + l %) (map - r %) :left])))
            (filter (comp #(and (legal-state? %)
                                (not (cannibalism? %)))
                          second)))))})

; (-> (solve missionaries-and-cannibals bfs)
;     clojure.pprint/pprint
;     time)


(pprint
  (diff '(def missionaries-and-cannibals
          {:init [[3 3] [0 0] :left]
           :goal? #{[[0 0] [3 3] :right]}
           :actions [[1 0] [2 0] [1 1] [0 2] [0 1]]
           :transitions
           (memoize
             (fn mc-transit [actions [l r p]]
               (->> actions
                    (map (if (= p :right)
                           (vector % [(map - l %) (map + r %) :dright])
                           (vector % [(map + l %) (map - r %) :left])))
                    (select (comp (and (legal-state? %)
                                        (not (cannibalism? %)))
                                  second)))))})
        '(def missionaries-and-cannibals
          {:init [[3 3] [0 0] :left]
           :goal? #{[[0 0] [3 3] :right]}
           :actions [[1 0] [2 0] [1 1] [0 2] [0 1]]
           :transitions
           (memoize
             (fn mc-transit [actions [l r p]]
               (->> actions
                    (map (if (= p :left)
                           (vector % [(map - l %) (map + r %) :right])
                           (vector % [(map + l %) (map - r %) :left])))
                    (filter (comp (and (legal-state? %)
                                        (not (cannibalism? %)))
                                  second)))))})))
