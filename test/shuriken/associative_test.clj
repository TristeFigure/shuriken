(ns shuriken.associative-test
  (:require [clojure.test :refer :all]
            [shuriken.core :refer :all]))

(defmacro assert-flatten-roundtrip [m]
  `(let [m# ~m]
     (is (= (deflatten-keys (flatten-keys m#))
            m#))))

(def m
  {:a {:b {:c 123
           :d 456}}
   :e {:f 789}})

(def flat-m
  {[:a :b :c] 123
   [:a :b :d] 456
   [:e :f]    789})

(deftest test-map-vals
  (is (= [[:a 2] [:b 3]]
         (map-vals inc [[:a 1] [:b 2]])
         (map-vals inc '([:a 1] [:b 2]))))
  (is (= {:a 2 :b 3}
         (map-vals inc {:a 1 :b 2})))
  (is (= [[:a 2]]
         (map-vals inc (lazy-seq [[:a 1]])))))

(deftest test-map-keys
  (is (= [["a" 1] ["b" 2]]
         (map-keys name [[:a 1] [:b 2]])
         (map-keys name '([:a 1] [:b 2]))))
  (is (= {"a" 1 "b" 2}
         (map-keys name {:a 1 :b 2})))
  (is (= [["a" 1]]
         (map-keys name (lazy-seq [[:a 1]])))))

(deftest test-flatten-keys
  (testing "flattening"
    (is (= (flatten-keys m)
           flat-m))
    (testing "of empty maps"
      (assert-flatten-roundtrip {}))
    (testing "of maps nesting empty maps"
      (assert-flatten-roundtrip {:a {}}))))

(deftest test-deflatten-keys
  (is (= (deflatten-keys flat-m)
         m)))

(deftest test-deep-merge
  (testing "deep-merge"
    (is (= (deep-merge m {:a {:b {:c :xyz}}})
           {:a {:b {:c :xyz
                    :d 456}}
            :e {:f 789}}))
    (testing "of three maps"
      (is (= (deep-merge {:a :aa}
                         {:a :aaa :b :bb}
                         {:b :bbb :c :cc})
             {:a :aaa
              :b :bbb
              :c :cc})))
    (testing "overriding"
      (is (= (deep-merge {:a :a  :b :b :c :c}
                         {:a :aa :b :bb}
                         {:a :aaa}
                         {})
             {:a :aaa :b :bb :c :c})))
    (testing "overriding nested maps"
      (is (= (deep-merge {:x {:a :a  :b :b  :c :c}}
                         {:x {:a :aa :b :bb}}
                         {:x {:a :aaa}})
             {:x {:a :aaa :b :bb :c :c}})))
    (testing "overriding empty nested maps"
      (is (= (deep-merge {:x 123}
                         {:x {}})
             {:x {}}))
      (is (= (deep-merge {:x {:y :z}}
                         {:x {:a :b}})
             {:x {:y :z :a :b}}))
      (is (= (deep-merge {:x {:y :z}}
                         {:x {}})
             {:x {:y :z}})))))

(deftest test-index-by
  (let [ms [{:a 1 :b 2} {:a 3 :b 4} {:a 5 :b 4}]]
    (testing "default strategy"
      (is (= (index-by :a ms)
             {1 {:a 1, :b 2}
              3 {:a 3, :b 4}
              5 {:a 5, :b 4}}))
      (is (thrown? clojure.lang.ExceptionInfo
                   (index-by :b ms))))
    (testing "custom strategy"
      (is (= (index-by :b #(last %2) ms)
             {2 {:a 1, :b 2}
              4 {:a 5, :b 4}})))))

(deftest test-merge-with-plan
  (let [m {:a 1 :b 2 :c inc :d 0}
        plan {:a + :b - :c comp}
        {:keys [a b c d] :as merged} (merge-with-plan plan m m m)]
    (is (map? merged))
    (is (= 3 a))
    (is (= -2 b))
    (is (= 3 (c 0)))
    (is (= 0 d))
    (is (= merged (merge-with-plan plan merged)))))

(deftest test-split-map
  (let [m {:a 1 :b 2 :c 3 :d 4}]
    (is (= [{:a 1 :b 2} {:c 3 :d 4}]   (split-map m [:a :b])))
    (is (= [{:a 1 :b 2} {:c 3} {:d 4}] (split-map m [:a :b] [:c])))
    (is (= [{:a 1 :b 2} {:c 3 :d 4}]   (split-map m [:a :b] [:c :d])))
    (is (= [{} {}]                     (split-map {} [:a :b] [:c :d])))
    (is (= [{} {}]                     (split-map nil [:a :b] [:c :d])))
    (is (= [m]                         (split-map m)))))

(deftest test-map-difference
  (let [m {:a 1 :b 2 :c 3}]
    (is (= {:a 1 :b 2 :c 3} (map-difference m {})))
    (is (= {:b 2 :c 3}      (map-difference m {:a :x})))
    (is (= {:c 3}           (map-difference m {:a :x :b :x})))
    (is (= {}               (map-difference m {:a :x :b :x :c :x})))
    (is (= {:c 3}           (map-difference m {:a :x} {:b :x})))))

(deftest test-submap?
  (let [m1 {:a 1}
        m2 {:a 1 :b 2}
        m3 {:a :x}
        m4 {:x 1}]
    (is (true?  (submap? m1 m1)))
    (is (true?  (submap? m1 m2)))
    (is (false? (submap? m2 m1)))
    (is (false? (submap? m1 m3)))
    (is (false? (submap? m1 m4)))))

(deftest test-getsoc
  (is (= [1 {:a 1}] (getsoc {:a 1} :a (constantly 2))))
  (is (= [2 {:a 2}] (getsoc {}     :a (constantly 2)))))
