(ns shuriken.waltz
  (:use clojure.pprint
        shuriken.debug)
  (:require [shuriken.debug :refer [debug-print]]
            [shuriken.fn :refer [arity]]))

(defn- no-walk? [form]
  (boolean
    (and (vector? form)
         (some-> form first #{::no-walk}))))

(defn no-walk [form]
  (if (no-walk? form)
    form
    [::no-walk form]))

(defn- unwalk [form]
  (second form))

(def ^:private debugs
  '{should-walk (when debug
                  (let [tabs (str (apply str (repeat level "  "))
                                  "- ")]
                    (if should-walk
                      (debug-print (str tabs "Walking     ") form)
                      (debug-print (str tabs "Not walking ") form))))
    pre-result  (when (and debug should-walk)
                  (if should-pre
                    (debug-print   (str tabs "Prewalked   ") pre-result)
                    (debug-print   (str tabs "No prewalk  ") pre-result)))
    unwalked    (when (and debug unwalked)
                  (debug-print     (str tabs "Unwalked    ") unwalked))
    result      (when debug
                  (if should-post
                    (debug-print   (str tabs "Postwalked  ") result)
                    (debug-print   (str tabs "No postwalk ") result)))})

(defmacro ^:private add-debugs [let-statement]
  (->> (update (vec let-statement) 1
               (fn [bindings]
                 (->> bindings
                      (partition 2)
                      (map (fn [[k v]]
                             (if-let [d (debugs k)]
                               [[k v]
                                ['_ d]]
                               [[k v]])))
                      (apply concat)
                      (apply concat)
                      vec)))
       (apply list)))
     
(defn- adapt-waltz-fn [f]
  (let [ar (arity f)]
    (case ar
      :variadic f
      2         f
      1         (fn
                  ([form]      (f form))
                  ([form _ctx] (f form))) ;; discard ctx
      (throw (Exception. (str "Wrong arity " ar))))))

;; TODO: add a context
(defn waltz
  "A finely tunable version of clojure.walk with enhancenments.
  
  The `walk?` option means that pre-processing and walking won't be
  performed while leaving the possibility of post-processing open.
  
  ```clojure
  (waltz
    '(+ 1 (- 2 (* 3 (/ 4 5) (dec 3))))
    :walk? #(and (seq? %) (-> % first (not= 'dec)))
    :pre?  #(and (seq? %) (-> % first (not= '/)))
    :pre   vec
    :post? number?
    :post  inc
    :debug true)
  
  ; - Walking     : (+ 1 (- 2 (* 3 (/ 4 5) (dec 3))))
  ;   Prewalked   : [+ 1 (- 2 (* 3 (/ 4 5) (dec 3)))]
  ;   - Not walking : +
  ;     No postwalk : +
  ;   - Not walking : 1
  ;     Postwalked  : 2
  ;   - Walking     : (- 2 (* 3 (/ 4 5) (dec 3)))
  ;     Prewalked   : [- 2 (* 3 (/ 4 5) (dec 3))]
  ;     - Not walking : -
  ;       No postwalk : -
  ;     - Not walking : 2
  ;       Postwalked  : 3
  ;     - Walking     : (* 3 (/ 4 5) (dec 3))
  ;       Prewalked   : [* 3 (/ 4 5) (dec 3)]
  ;       - Not walking : *
  ;         No postwalk : *
  ;       - Not walking : 3
  ;         Postwalked  : 4
  ;       - Walking     : (/ 4 5)
  ;         No prewalk  : (/ 4 5)
  ;         - Not walking : /
  ;           No postwalk : /
  ;         - Not walking : 4
  ;           Postwalked  : 5
  ;         - Not walking : 5
  ;           Postwalked  : 6
  ;         No postwalk : (/ 5 6)
  ;       - Not walking : (dec 3)
  ;         No postwalk : (dec 3)
  ;       No postwalk : [* 4 (/ 5 6) (dec 3)]
  ;     No postwalk : [- 3 [* 4 (/ 5 6) (dec 3)]]
  ;   No postwalk : [+ 2 [- 3 [* 4 (/ 5 6) (dec 3)]]]
  => [+ 2 [- 3 [* 4 (/ 5 6) (dec 3)]]]
  ```"
  [form & [& {:keys [walk? pre? pre post? post debug level]
              :or {walk? any?
                   pre?  any?  pre   identity
                   post? any?  post  identity
                   debug false level 0}}]]
  (add-debugs
    (let [opts (->> {:walk? walk?
                     :pre?  pre?  :pre   pre
                     :post? post? :post  post
                     :debug debug :level level}
                    (map (fn [[k f]]
                           (if (#{:debug :level} k)
                             [k f]
                             [k (adapt-waltz-fn f)])))
                    (into {}))
          tabs          (apply str (repeat (inc level) "  "))
          should-walk   (walk? form)
          should-pre    (pre? form)
          pre-result    (if (and should-walk should-pre) (pre form) form)
          unwalked      (when (no-walk? pre-result) (unwalk pre-result))
          out           (if (and should-walk (not unwalked))
                          (clojure.walk/walk
                            #(apply waltz %
                                    (apply concat (update opts :level inc)))
                            identity
                            pre-result)
                          (if unwalked unwalked pre-result))
          should-post   (post? out)
          result        (if should-post (post out) out)]
      result)))
