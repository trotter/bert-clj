(ns com.trottercashion.bert-clj.etf-decoder-test
  (:use clojure.contrib.test-is
        com.trottercashion.bert-clj.test-helper)
  (:require [com.trottercashion.bert-clj.etf-decoder :as decoder]
            [com.trottercashion.bert-clj.etf-encoder :as encoder]
            [clojure.contrib.math :as math]))

(defn test-round-trip [val]
  (is (= (decoder/decode (encoder/encode val)) val)))

(deftest should-decode-string
  (test-round-trip "hi there, bob"))

(deftest should-decode-float
  (let [val 4000.0003]
    (is (> 0.001 (math/abs (- (decoder/decode (encoder/encode val)) val))))))

(deftest should-decode-small-integer
  (test-round-trip 234))

(deftest should-decode-big-integer
  (test-round-trip 4000))

(deftest should-decode-negative-big-integer
  (test-round-trip -1))