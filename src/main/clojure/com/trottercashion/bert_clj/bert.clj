(ns com.trottercashion.bert-clj.bert
  (:require [com.trottercashion.bert-clj.etf-encoder  :as etf-encoder]
            [com.trottercashion.bert-clj.bert-encoder :as bert-encoder]
            [com.trottercashion.bert-clj.etf-decoder  :as etf-decoder]))

(defn encode [obj]
  (etf-encoder/encode (bert-encoder/encode obj)))

(defn decode [coll]
  (etf-decoder/decode coll))

