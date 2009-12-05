(ns com.trottercashion.bert-clj.converter-test
  (:use clojure.contrib.test-is)
  (:require [com.trottercashion.bert-clj.converter :as converter]))

(deftest should-convert-nil
  (let [expected ['bert 'nil]]
    (is (= (converter/convert nil) expected))))

(deftest should-convert-boolean
  (is (= (converter/convert true) ['bert 'true]))
  (is (= (converter/convert false) ['bert 'false])))

(deftest should-convert-map
  (let [expected ['bert 'dict "hello" "mom" :nine 7]]
    (is (= (converter/convert {"hello" "mom" :nine 7}) expected))))