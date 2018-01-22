(ns shuriken.destructure-test
  (:require [clojure.test :refer :all]
            [shuriken.destructure :refer :all]))

;; TODO: require shuriken.core instead of shuriken.destructure

(deftest test-disentangle
  (testing "when params is an array"
    (is (= '{:items [a b]}
           (disentangle '[a b])))
    (is (= '{:items [[a b]]}
           (disentangle '[[a b]])))
    (is (= '{:items [], :more args}
           (disentangle '[& args])))
    (is (= '{:items [a b], :more {:keys [x], y :_y, :or {x 1}, :as m}}
           (disentangle '[a b & {:keys [x] y :_y :or {x 1} :as  m}]))))
  (testing "when params is a hash"
    (is (= '{:items [a b [c1 c2]],
             :as m,
             :or {d 1},
             :mapping {a :a, b :b, [c1 c2] :c}}
           (disentangle '{:keys [a] b :b [c1 c2] :c :or {d 1} :as m})))))

(deftest test-deconstruct
  (testing "when params is an array"
    (is (= '[a x y m]
           (deconstruct '[a & {:keys [x] y :_y :or {x 1} :as m}])))
    (is (= '[args]
        (deconstruct '[& args]))))
  (testing "when params is a hash"
    (is (= '[a]
           (deconstruct '{a :a})))
    (is (= '[a b m]
           (deconstruct '{:keys [a] b :b :or {a 1} :as m})))))

(deftest test-restructure
  (testing "when params is an array"
    (is (= [1 2]        (restructure '[a b]               '[a 1 b 2])))
    (is (= [1 2 {:c 3}] (restructure '[a b {:keys [c]}]   '[a 1 b 2 c 3])))
    (is (= [1 2 :c 3]   (restructure '[a b & {:keys [c]}] '[a 1 b 2 c 3])))
    (is (= [[1 2 :x "xx", :_y [:j :k :l]]]
           (restructure '[[a b & {:keys [x] [j k l] :_y :as opts}]]
                        '[a 1 b 2 x "xx"  j :j k :k l :l])))
    (testing "with &"
      (testing "followed by a symbol"
        (testing "and the symbol turns out to be a sequential coll"
          (is (= [1 2 3]       (restructure '[a & args] '[a 1 args [2 3]]))))
        (testing "and the symbol turns out to be a map"
          (is (= [1 :b 2 :c 3] (restructure '[a & args]
                                            '[a 1 args {:b 2 :c 3}])))))
      (testing "followed by more destructuring"
        (is   (= [1 2 3  ]     (restructure '[a & [b & c]] '[a 1 b 2 c [3]]))))))
  (testing "when params is a hash"
    (is (= {:a 1 :b 2}  (restructure '{:keys [a b]} '[a 1 b 2])))
    (is (= {:a 1 :bb 2} (restructure '{a :a b :bb}  '[a 1 b 2])))
    (testing "with :or parameter"
      (is (= {:a 1, :bb 2, :c 3}
             (restructure '{:keys [a] b :bb c :c :or {c 3}} '[a 1 b 2])))))
  (testing "when mapping is a function"
    (is (= {:a "a", :b ["x" "y"]} (restructure '{:keys [a] [x y] :b} str))))
  (testing "when mapping is a hash"
    (is (= [1] (restructure '[a] '{a 1})))))

(deftest destructure-restructure-roundtrip
  (let [params   '[a b & {:keys [x] [j k l] :_y :as opts}]
        mapping '[a 1 b 2 x "xx" j :j k :k l :l]]
    (is (= (eval `(let [~params (restructure '~params '~mapping)]
                    ~(->> (partition 2 mapping)
                          (mapcat (fn [[k _v]] `[(quote ~k) ~k]))
                          vec)))
           mapping))))