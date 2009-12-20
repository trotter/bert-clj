(ns com.trottercashion.bert-clj.encoding-test
  (:use clojure.contrib.test-is)
  (:require [com.trottercashion.bert-clj.bert :as bert]
            [com.trottercashion.bert-clj.etf-encoder :as etf-encoder]
            [com.trottercashion.bert-clj.bert-encoder :as bert-encoder]))

(defn test-round-trip [val]
  (is (= (bert/decode (bert/encode val)) val)))

(deftest should-encode-etf-type
  (is (= (bert/encode "hello") (etf-encoder/encode "hello"))))

(deftest should-encode-complex-types
  (is (= (bert/encode {"one key" "two key" "red key" "blue key"})
         (etf-encoder/encode (bert-encoder/encode {"one key" "two key" "red key" "blue key"})))))

(deftest should-decode-etf-type
  (test-round-trip "hello"))

(deftest should-decode-complex-types
  (test-round-trip {"one key" "two key" "red key" "blue key"}))

(deftest should-roundtrip-nil
  (test-round-trip nil))