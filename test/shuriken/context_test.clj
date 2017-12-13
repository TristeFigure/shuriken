(ns shuriken.context-test
  (:require [clojure.test :refer :all]
            [shuriken.context :refer :all]))

(deftest test-store-then-bind-contextx
  (let [x 1 y 2] (context! :key)
    (is (= [1 2]
           (binding-stored-locals :key [x y]))))
  (let [expect-no-symbol
        (fn [sym code]
          (is (= (format "Unable to resolve symbol: %s in this context" sym)
                 (try
                   (eval code)
                   (catch Throwable t
                     (-> t .getCause .getMessage))))))]
    (testing "Filtering the locals"
      (expect-no-symbol
        'z `(do (let [~'x 1 ~'y 2 ~'z 100]
                  (store-locals! :key (complement '#{~'z})))
             (binding-stored-locals :key ~'z))))
    (testing "Deleting stored locals"
      (expect-no-symbol
        'x `(do (let [~'x 1]
                  (store-locals! :key)
                  (delete-stored-locals! :key))
             (binding-stored-locals :key ~'x))))))
