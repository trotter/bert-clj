(ns com.trottercashion.bert-clj.bert
  (:require [com.trottercashion.bert-clj.etf-encoder  :as etf-encoder]
            [com.trottercashion.bert-clj.bert-encoder :as bert-encoder]))

(defn encode [obj]
  (etf-encoder/encode (bert-encoder/encode obj)))
