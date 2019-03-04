(ns shuriken.macro-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all :as test]
            [clojure.java.io :as io]
            [shuriken.macro :refer :all]
            [shuriken.core :refer [thrown?]]
            [shuriken.namespace :refer [with-ns]]))

;; TODO: require shuriken.core instead of shuriken.macro

(defn- clean-generated-files [test-f]
  (try (test-f)
    (finally
      (doseq [f (reverse (file-seq (io/file *macroexpansions-dir*)))]
        (.delete f)))))

(use-fixtures :once clean-generated-files)

(defmacro bb [code]
  `(+ 2 (b ~code)))

(defmacro b [code]
  `(inc ~code))

(defmacro a [code]
  `(do (b ~code)
       (bb ~code)))

(defmacro mm [] :abc)

(deftest test-is-form?
  (is (= true  (is-form? 'a '(a :z))))
  (is (= false (is-form? 'x '(a :z))))
  (is (= false (is-form? 'x 123)))
  (is (= false (is-form? 'a '[a :z]))))

(deftest test-wrap-form-unwrap-form
  (is (= '(a :z)
         (wrap-form 'a :z)
         (wrap-form 'a (wrap-form 'a :z))))
  (is (= :z
         (unwrap-form 'a '(a :z))
         (unwrap-form 'a (unwrap-form 'a '(a :z)))))
  (is (= :z
         (->> :z
              (wrap-form 'a)
              (unwrap-form 'a)))))

(deftest test-clean-code
  (testing "Asserting backtick standard behavior"
    (is (not= '(x (y z)) ``~(x (y z))))
    (is (not= 'Object/staticMeth `Object/staticMeth)))
  (is (= '(x (y z) java.lang.Object/staticMeth)
         (with-ns 'shuriken.macro-test
           (clean-code '(shuriken.macro-test/x
                                (shuriken.macro-test/y shuriken.macro-test/z)
                                java.lang.Object/staticMeth))))))

(def test-folder
  "./src/macroexpansions/shuriken/macro_test.clj")

(defn list-files [folder]
  (-> (io/file folder)
      .listFiles
      seq))

(defn delete-folder [folder]
  (let [folder (io/file folder)]
    (when (.exists folder)
      (doseq [f (.listFiles folder)]
        (if (.isDirectory f)
          (delete-folder f)
          (.delete f)))
      (.delete folder))))

(deftest test-file-eval
  (binding [*file* "shuriken/macro_test.clj"]
    (with-ns 'shuriken.macro-test
      (is (= 2 (let [x 1] (file-eval '(+ 1 x)))))
      (is (= 2 (let [x 1] (file-eval (+ 1 x)))))
      (let [x 1
            code '(+ 1 x)]
        (is (= 2 (file-eval code))))
      (testing "persistence"
        (let [code '(/ 1 x)
              _   (delete-folder test-folder)
              _ (println "FILE" *file*)
              ok  (is (= 1 (let [x 1] (file-eval code))))
              _   (is (empty? (list-files test-folder)))
              ko  (is (true? (thrown? ArithmeticException
                                      (let [x 0] (file-eval code)))))
              _   (is (= 1 (count (list-files test-folder))))
              ;; TODO
              ; fix (spit (-> test-folder list-files first)
              ;           "(/ 1 2)")
              ; ok  (is (= 1/2 (let [x 0] (file-eval code))))
              ; _   (is (empty? (list-files test-folder)))
              ]
          (delete-folder test-folder))))))

(deftest test-macroexpand-all-code
  (testing "asserting clojure.walk/macroexpand-all's behavior"
    (is (= '(:abc (quote :abc))
           (clojure.walk/macroexpand-all '((shuriken.macro-test/mm)
                                           (quote (shuriken.macro-test/mm)))))))
  (is (= '(:abc (quote (shuriken.macro-test/mm)))
           (macroexpand-all-code '((shuriken.macro-test/mm)
                                    (quote (shuriken.macro-test/mm)))))))

(deftest test-macroexpand-some
  (is (= '(do (do (shuriken.macro-test/b (+ 1 2))
                  (shuriken.macro-test/bb (+ 1 2))))
         (macroexpand-some '#{a} '(do (shuriken.macro-test/a (+ 1 2))))))
  (is (= '(do (clojure.core/inc (+ 1 2))
           (shuriken.macro-test/bb (+ 1 2)))
         (macroexpand-some '#{a b} '(shuriken.macro-test/a (+ 1 2)))))
  (is (= '(do (clojure.core/inc (+ 1 2))
           (clojure.core/+ 2 (clojure.core/inc (+ 1 2))))
         (macroexpand-some '#{a b bb} '(shuriken.macro-test/a (+ 1 2)))))
  (is (= '(let [x (do (shuriken.macro-test/b 1) (shuriken.macro-test/bb 1))] x)
         (macroexpand-some '#{a} '(let [x (shuriken.macro-test/a 1)] x)))))

(deftest test-macroexpand-n
  (is (= (macroexpand-1  '(a 1))
         (macroexpand-n 1 '(a 1)))))

(deftest test-macroexpand-depth
  (is (= `(do (inc 123) (+ 2 (b 123)))
         (macroexpand-depth 1 '(shuriken.macro-test/a 123)))))

(defmacro bogus-compilation []
  (/ 1 0))

(defmacro bogus-execution []
  `(/ 1 0))

(defmacro ^:private silencing [expr]
  (binding [*out* (new java.io.StringWriter)]
    `(binding [*out* (new java.io.StringWriter)]
       ~(macroexpand expr))))

(deftest test-macroexpand-do
  (with-ns 'shuriken.macro-test
    (testing "unquoted & bogus at compile time"
      (is (true? (thrown? #"Divide by zero"

                          (silencing
                            (macroexpand-do
                              (bogus-compilation)))))))
    (testing "quoted & bogus at run time"
      (is (true? (thrown? #"Divide by zero"
                          (silencing
                            (macroexpand-do
                              '(bogus-execution)))))))))
