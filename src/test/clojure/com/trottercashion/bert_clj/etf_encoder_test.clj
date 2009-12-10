(ns com.trottercashion.bert-clj.etf-encoder-test
  (:use clojure.contrib.test-is
        com.trottercashion.bert-clj.test-helper)
  (:require [com.trottercashion.bert-clj.etf-encoder :as encoder]))

(deftest should-encode-string
  (let [expected (binary 131 107 0 5 104 101 108 108 111)]
    (is (= (encoder/encode "hello") expected))))

(deftest should-encode-float
  (let [expected (binary 131 99 53 46 53 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 101 43 48 48)]
    (is (= (encoder/encode 5.5) expected))))

(deftest should-encode-small-integer
  (let [expected (binary 131 97 4)]
    (is (= (encoder/encode 4) expected))))

(deftest should-encode-big-integer
  (let [expected (binary 131 98 0 0 15 160)]
    (is (= (encoder/encode 4000) expected))))

(deftest should-encode-negative-big-integer
  (let [expected (binary 131 98 255 255 255 255)]
    (is (= (encoder/encode -1) expected))))

(deftest should-encode-symbols-atoms
  (let [expected (binary 131 100 0 7 116 114 111 116 116 101 114)]
    (is (= (encoder/encode 'trotter) expected))))

(deftest should-encode-keywords-as-atoms
  (let [expected (binary 131 100 0 7 116 114 111 116 116 101 114)]
    (is (= (encoder/encode :trotter) expected))))

(deftest should-encode-small-tuple
  (let [expected (binary 131 104 2 100 0 5 104 101 108 108 111 100 0 5 119 111 114 108 100)]
    (is (= (encoder/encode (vector 'hello 'world)) expected))))

(deftest should-encode-large-tuple
  (let [expected (concat (binary 131 105 0 0 1 0) (apply concat (take 256 (repeat '(97 2)))))]
    (is (= (encoder/encode (vec (take 256 (repeat 2)))) expected))))

(deftest should-encode-empty-list
  (let [expected (binary 131 106)]
    (is (= (encoder/encode '()) expected))))

(deftest should-encode-lists
  (let [expected (binary 131 108 0 0 0 3 107 0 1 97 107 0 1 98 107 0 1 99 106)]
    (is (= (encoder/encode '("a" "b" "c")) expected))))

(deftest should-encode-lists-when-lazy
  (let [expected (binary 131 108 0 0 0 3 107 0 1 97 107 0 1 98 107 0 1 99 106)]
    (is (= (encoder/encode (lazy-seq '("a" "b" "c"))) expected))))

(deftest should-encode-binary-lists
  (let [expected (binary 131 109 0 0 0 3 1 2 3)]
    (is (= (encoder/encode (binary 1 2 3)) expected))))

(deftest should-encode-small-bignums
  (let [min-expected (binary 131 110 4 0 0 0 0 128)
        max-expected (concat (binary 131 110 255 1) (map byte (take 255 (repeat 255))))
        min-small-bignum (bit-shift-left 1 31)
        max-small-bignum -126238304966058622268417487065116999845484776053576109500509161826268184136202698801551568013761380717534054534851164138648904527931605160527688095259563605939964364716019515983399209962459578542172100149937763938581219604072733422507180056009672540900709554109516816573779593326332288314873251559077853068444977864803391962580800682760017849589281937637993445539366428356761821065267423102149447628375691862210717202025241630303118559188678304314076943801692528246980959705901641444238894928620825482303431806955690226308773426829503900930529395181208739591967195841536053143145775307050594328881077553168201547775]
    (is (= (encoder/encode min-small-bignum) min-expected))
    (is (= (encoder/encode max-small-bignum) max-expected))))

(deftest should-encode-large-bignums
  (let [expected (concat (binary 131 111 0 0 1 0 0) (map byte (take 255 (repeat 0))) [(byte 1)])
        large-bignum (bit-shift-left 1 (* 255 8))]
    (is (= (encoder/encode large-bignum) expected))))

