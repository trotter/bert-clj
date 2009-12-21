(ns com.trottercashion.bert-clj.etf-decoder-test
  (:use clojure.contrib.test-is
        com.trottercashion.bert-clj.test-helper)
  (:require [com.trottercashion.bert-clj.etf-decoder :as decoder]
            [com.trottercashion.bert-clj.etf-encoder :as encoder]
            [clojure.contrib.math :as math]))

(defn test-round-trip [val]
  (let [encoded (encoder/encode val)]
    (is (= (decoder/decode-with-size encoded) [val (count encoded)]))))

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

(deftest should-decode-atom
  (test-round-trip :trotter))

(deftest should-decode-small-tuple
  (test-round-trip (vector :hello :world)))

(deftest should-decode-tuple-with-inner-tuple
  (test-round-trip (vector (vector 2 3) 4)))

(deftest should-decode-large-tuple
  (test-round-trip (vec (take 256 (repeat 2)))))

(deftest should-decode-empty-list
  (test-round-trip '()))

(deftest should-decode-lists
  (test-round-trip '(1 2 3)))

(deftest should-decode-binary-lists
  (test-round-trip (map byte '(1 2 3))))

(deftest should-decode-small-bignums
  (test-round-trip (bit-shift-left 1 31)))

(deftest should-decode-large-bignums
  (test-round-trip (bit-shift-left 1 (* 255 8))))