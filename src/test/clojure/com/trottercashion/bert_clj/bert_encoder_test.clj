(ns com.trottercashion.bert-clj.bert-encoder-test
  (:use clojure.contrib.test-is)
  (:require [com.trottercashion.bert-clj.bert-encoder :as encoder]))

(deftest should-encode-nil
  (let [expected [:bert :nil]]
    (is (= (encoder/encode nil) expected))))

(deftest should-encode-boolean
  (is (= (encoder/encode true) [:bert :true]))
  (is (= (encoder/encode false) [:bert :false])))

(deftest should-encode-dict
  (let [expected [:bert :dict '(["hello" "mom"] [:nine 7])]]
    (is (= (encoder/encode {"hello" "mom" :nine 7}) expected))))

(deftest should-encode-time
  (let [milliseconds 12345678912345
        expected [:bert :time 12345 678912 345000]]
    (is (= (encoder/encode (java.util.Date. milliseconds)) expected))))

;; Mapping of erlang to java regexp options is in order below
;; Java default for regex is to match \r\n, whereas erlang is \n, so we're leaving off the ?d flag in this test
(deftest should-encode-regex-with-lf-line-endings
  (let [expected [:bert :regex "c(a*)t$" '(caseless extended multiline dotall unicode)]]
    (is (= (encoder/encode #"(?ixmsud)c(a*)t$") expected))))

(deftest should-encode-regex-with-crlf-endings
  (let [expected [:bert :regex "c(a*)t$", '([newline anycrlf])]]
    (is (= (encoder/encode #"c(a*)t$") expected))))