(ns com.trottercashion.bert-clj.etf-decoder-test
  (:use clojure.contrib.test-is
        com.trottercashion.bert-clj.test-helper)
  (:require [com.trottercashion.bert-clj.etf-decoder :as decoder]
            [com.trottercashion.bert-clj.etf-encoder :as encoder]))

(deftest should-decode-string
  (is (= (decoder/decode (encoder/encode "hello")) "hello")))