(ns com.trottercashion.bert-clj.converter-test
  (:use clojure.contrib.test-is)
  (:require [com.trottercashion.bert-clj.converter :as converter]))

(deftest should-convert-nil
  (let [expected {'bert, nil}]
    (is (= (converter/convert nil) expected))))