(ns com.trottercashion.bert-clj.bert-decoder-test
  (:use clojure.contrib.test-is)
  (:require [com.trottercashion.bert-clj.bert-decoder :as decoder]))

(deftest should-decode-nil
  (is (= (decoder/decode [:bert :nil]) nil)))

(deftest should-decode-boolean
  (is (= (decoder/decode [:bert :true]) true))
  (is (= (decoder/decode [:bert :false]) false)))

(deftest should-decode-dict
  (is (= (decoder/decode [:bert :dict '(["hello" "mom"] [:nine 7])])
         {"hello" "mom" :nine 7})))

(deftest should-decode-time
  (is (= (decoder/decode [:bert :time 12345 678912 345000])
         (java.util.Date. 12345678912345))))

(deftest should-decode-regex-with-lf-line-endings
  (let [actual   (decoder/decode [:bert :regex "c(a*)t$" '(caseless extended multiline dotall unicode)])
        expected (java.util.regex.Pattern/compile "c(a*)t$"
                                                  (reduce bit-or [java.util.regex.Pattern/CASE_INSENSITIVE
                                                                  java.util.regex.Pattern/COMMENTS
                                                                  java.util.regex.Pattern/MULTILINE
                                                                  java.util.regex.Pattern/DOTALL
                                                                  java.util.regex.Pattern/UNICODE_CASE
                                                                  java.util.regex.Pattern/UNIX_LINES]))]
    (is (= (.pattern expected) (.pattern actual)))
    (is (= (.flags expected) (.flags actual)))))

(deftest should-decode-regex-with-crlf-endings
  (let [actual   (decoder/decode [:bert :regex "c(a*)t$" '([newline anycrlf])])
        expected (java.util.regex.Pattern/compile "c(a*)t$")]
    (is (= (.pattern expected) (.pattern actual))
        (= (.flags   expected) (.flags   actual)))))