(ns shuriken.navigation-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer [tree-seq-breadth]]))

(def tree
  {:a {:d {:j :_}
       :e {:k :_}}
   :b {:f {:l :_}
       :g {:m :_}}
   :c {:h {:n :_}
       :i {:o :_}}})

(deftest test-breadth-tree-seq
  (testing "it works"
    (let [result (tree-seq-breadth map? vals tree)]
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
