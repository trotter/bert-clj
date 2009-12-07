(ns com.trottercashion.bert-clj.bert-encoder-test
  (:use clojure.contrib.test-is)
  (:require [com.trottercashion.bert-clj.bert-encoder :as encoder]))

(deftest should-encode-nil
  (let [expected ['bert 'nil]]
    (is (= (encoder/encode nil) expected))))

(deftest should-encode-boolean
  (is (= (encoder/encode true) ['bert 'true]))
  (is (= (encoder/encode false) ['bert 'false])))

(deftest should-encode-map
  (let [expected ['bert 'dict '(["hello" "mom"] [:nine 7])]]
    (is (= (encoder/encode {"hello" "mom" :nine 7}) expected))))

(deftest should-encode-time
  (let [milliseconds 12345678912345
        expected ['bert 'dict 12345 678912 345000]]
    (is (= (encoder/encode (java.util.Date. milliseconds)) expected))))