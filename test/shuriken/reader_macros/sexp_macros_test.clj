(ns shuriken.reader-macros.sexp-macros-test
  (:require [clojure.test :refer :all]
            [shuriken.reader-macros.sexp-macros :refer :all])
  (:import [java.nio CharBuffer]
           [java.io StringReader]))

(deftest test-dynamic-pushback-reader
  (let [r   #(dynamic-pushback-reader (StringReader. "xyz"))]
    (testing "reading one char"
      (is (= \x (char (.read (r))))))
    (testing "reading chars into a char-array"
      (let [buf (char-array 3)]
          (.read (r) buf)
          (is (= [\x \y \z] (seq buf)))))
    (testing "reading chars into a java.nio.CharBuffer"
      (let [buf (CharBuffer/allocate 3)]
        (.read (r) buf)
        (.flip buf)
        (is (= "xyz" (.toString buf)))))
    (testing "reading chars into a portion of a char-array"
      (let [buf (char-array 3)]
          (.read (r) buf 2 1)
          (is (= [\u0000 \u0000 \x] (seq buf)))))
    (testing "skipping chars"
      (let [buf (char-array 3)
            r   (r)]
        (.skip r 2)
        (is (= \z (char (.read r)))))
      (testing "edge case: zero chars skipped"
        (let [buf (char-array 3)
              r   (r)]
          (.skip r 0)
          (.read r buf)
          (is (= [\x \y \z] (seq buf))))))
    (testing "unreading a char"
      (let [buf (char-array 3)
            r   (r)]
        (.read r)
        (.unread r (int \a))
        (is (= (int \a) (.read r)))))
    (testing "unreading a char-array"
      (let [buf (char-array 6)
            r   (r)]
        (.unread r (char-array [\a \b \c]))
        (.read r buf)
        (is (= [\a \b \c \x \y \z]
               (seq buf)))))
    (testing "unreading a string"
      (let [buf (char-array 6)
            r   (r)]
        (.unreadString r "abc")
        (.read r buf)
        (is (= [\a \b \c \x \y \z]
               (seq buf)))))))

(deftest test-unread-string
  (let [r   (StringReader. "xyz")
        dpr (unread-string r "abc")
        buf (char-array 6)]
    (.read dpr buf)
    (is (= [\a \b \c \x \y \z] (seq buf)))))

(run-tests)
