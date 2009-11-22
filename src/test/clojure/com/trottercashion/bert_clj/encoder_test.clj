(ns com.trottercashion.bert-clj.encoder-test
  (:use clojure.contrib.test-is)
  (:require [com.trottercashion.bert-clj.encoder :as encoder]))

(defn binary [& args] (map byte args))

(deftest should-encode-string
  (let [expected (binary 131 107 0 5 104 101 108 108 111)]
    (is (= (encoder/encode "hello") expected))))

(deftest should-encode-float
  (let [expected (binary 131 99 53 46 53 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 101 43 48 48)]
    (is (= (encoder/encode 5.5) expected))))

(deftest should-encode-small-integer
  (let [expected (binary 131 97 4)]
    (is (= (encoder/encode 4) expected))))

(deftest should-encode-big-integer
  (let [expected (binary 131 98 0 0 15 160)]
    (is (= (encoder/encode 4000) expected))))

(deftest should-encode-atoms
  (let [expected (binary 131 100 0 7 116 114 111 116 116 101 114)]
    (is (= (encoder/encode 'trotter) expected))))

(deftest should-encode-nil
  (let [expected (binary 131 106)]
    (is (= (encoder/encode nil) expected))))

(deftest should-encode-lists
  (let [expected (binary 131 108 0 0 0 3 107 0 1 97 107 0 1 98 107 0 1 99 106)]
    (is (= (encoder/encode '("a" "b" "c")) expected))))

