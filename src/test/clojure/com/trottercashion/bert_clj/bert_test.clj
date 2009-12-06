(ns com.trottercashion.bert-clj.encoding-test
  (:use clojure.contrib.test-is)
  (:require [com.trottercashion.bert-clj.bert :as bert]
            [com.trottercashion.bert-clj.etf-encoder :as etf-encoder]
            [com.trottercashion.bert-clj.bert-encoder :as bert-encoder]))

(deftest should-encode-etf-type
  (is (= (bert/encode "hello") (etf-encoder/encode "hello"))))

(deftest should-encode-complex-types
  (is (= (bert/encode {"one key" "two key" "red key" "blue key"})
         (etf-encoder/encode (bert-encoder/encode {"one key" "two key" "red key" "blue key"})))))
