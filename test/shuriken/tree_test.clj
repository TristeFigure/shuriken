(ns shuriken.tree-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all]))

(def a-tree
  {:a {:d {:j :_}
       :e {:k :_}}
   :b {:f {:l :_}
       :g {:m :_}}
   :c {:h {:n :_}
       :i {:o :_}}})

(deftest test-breadth-tree-seq
  (testing "it works"
    (let [result (tree-seq-breadth map? vals a-tree)]
      (is (= '(:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o)
             (->> result
                  (remove #{:_})
                  (mapcat keys))))))
  (testing "edge cases"
    (is (= (tree-seq-breadth map? vals {})
           '({})))
    (is (= (tree-seq-breadth map? vals {:a {}})
           '({:a {}} {})))
    (is (= (tree-seq-breadth map? vals {{} :a})
           '({{} :a} :a)))))

(defn inc-it [x]
  (if (number? x) (inc x) x))

(defn it-times-two [x]
  (if (number? x) (* 2 x) x))

(def data
  [1 2 {3 4 5 6}])

(deftest test-prepostwalk
  (is (= (prepostwalk inc-it it-times-two data)
         [4 6 {8 10 12 14}]))
  (is (= (prepostwalk it-times-two inc-it data)
         [3 5 {7 9 11 13}])))

(defn symmetric? [xs]
  (let [cnt (Math/floor (/ (count xs) 2))]
    (= (take cnt xs)
       (take cnt (reverse xs)))))

(deftest test-prepostwalk-demo
  (let [visited (-> (with-out-str (prepostwalk-demo data))
                  (clojure.string/replace #"Walked (into|out of):\s*" "")
                  (clojure.string/split #"\n"))
        cnt (count visited)]
    (is (= visited
           ["[1 2 {3 4, 5 6}]"
            "1"
            "1"
            "2"
            "2"
            "{3 4, 5 6}"
            "[3 4]"
            "3"
            "3"
            "4"
            "4"
            "[3 4]"
            "[5 6]"
            "5"
            "5"
            "6"
            "6"
            "[5 6]"
            "{3 4, 5 6}"
            "[1 2 {3 4, 5 6}]"]))))

(deftest test-tree
  (let [divisors (tree #(for [m (range 2 %)
                              :let [div (/ % m)]
                              :when (integer? div)]
                          div)
                       cons
                       12)]
    (is (= '(12 (6 (3) (2)) (4 (2)) (3) (2))
           divisors))))
