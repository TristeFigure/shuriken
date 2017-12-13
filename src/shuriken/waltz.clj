(ns shuriken.waltz
  (require [shuriken.threading :refer :all]
           [shuriken.macro :refer [is-form?]]))

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

(defn waltz [form & [& {:keys [pre post walk? process? debug level]
                             :or {pre      identity
                                  post     identity
                                  walk?    (constantly true)
                                  unwalk   identity
                                  process? (constantly true)
                                  debug   false
                                  level    0}}
                          :as opts]]
  (let [tabs (apply str (repeat level ""))
        with-debug (fn [str f]
                     (fn [& args]
                       (println "waltz: ")))
        _ (newline)
        _ (println "form:"  form)
        _ (println "walk?:" (walk? form))
        pre-result (if-> form walk? pre)
        _ (println "pre-result:" pre-result)
        post-result (if (no-walk? pre-result)
                      (unwalk pre-result)
                      (post (clojure.walk/walk #(apply waltz % opts)
                                               identity
                                               pre-result)))]
    post-result))


(println "--->"
         (waltz
           '(+ 1 (- 2 (* 3 (/ 4 5)
                           (dec 3))))
           :walk? #(is-form? 'dec %)
           ; :process? (complement (partial is-form? '-))
           :pre (fn truc [x]
                  (if->> x (is-form? '/) no-walk))
           :post #(if-> % number? inc)
           ; :debug true
           ))
