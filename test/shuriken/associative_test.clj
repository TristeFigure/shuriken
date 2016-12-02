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
    (testing "overriding of nested maps"
      (is (= (deep-merge {:x {:a :a  :b :b  :c :c}}
                         {:x {:a :aa :b :bb}}
                         {:x {:a :aaa}})
             {:x {:a :aaa :b :bb :c :c}})))
    (testing "overriding of empty nested maps"
      (is (= (deep-merge {:x 123}
                         {:x {}})
             {:x {}}))
      (is (= (deep-merge {:x {:y :z}}
                         {:x {:a :b}})
             {:x {:y :z :a :b}}))
      (is (= (deep-merge {:x {:y :z}}
                         {:x {}})
             {:x {:y :z}})))))

(defmacro thrown? [exception-class & body]
  `(try
     (do ~@body
         false)
     (catch ~exception-class
       ~@body)))

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
