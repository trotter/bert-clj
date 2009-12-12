(ns com.trottercashion.bert-clj.bert-decoder-test
  (:use clojure.contrib.test-is)
  (:require [com.trottercashion.bert-clj.bert-decoder :as decoder]))

(deftest should-decode-nil
  (is (= (decoder/decode ['bert 'nil]) nil)))

(deftest should-decode-boolean
  (is (= (decoder/decode ['bert 'true]) true))
  (is (= (decoder/decode ['bert 'false]) false)))

(deftest should-decode-dict
  (is (= (decoder/decode ['bert 'dict '(["hello" "mom"] [:nine 7])])
         {"hello" "mom" :nine 7})))

(deftest should-decode-time
  (is (= (decoder/decode ['bert 'time 12345 678912 345000])
         (java.util.Date. 12345678912345))))
