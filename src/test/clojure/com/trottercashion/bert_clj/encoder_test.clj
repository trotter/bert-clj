(ns com.trottercashion.bert-clj.encoder-test
  (:use clojure.contrib.test-is)
  (:require [com.trottercashion.bert-clj.encoder :as encoder]))

(defn binary [& args] (map #(byte %) args))

(deftest should-encode-string
  (let [expected (binary 131 107 0 5 104 101 108 108 111)]
    (is (= (encoder/encode "hello") expected))))

(deftest should-encode-float
  (let [expected (binary 131 99 53 46 53 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 101 43 48 48)]
    (is (= (encoder/encode 5.5) expected))))

(deftest should-encode-small-integer
  (let [expected (binary 131 97 4)]
    (is (= (encoder/encode 4) expected))))
