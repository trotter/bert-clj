(ns com.trottercashion.bert-clj.bert
  (:require [com.trottercashion.bert-clj.etf-encoder  :as etf-encoder]
            [com.trottercashion.bert-clj.bert-encoder :as bert-encoder]
            [com.trottercashion.bert-clj.etf-decoder  :as etf-decoder]))

(defn encode [obj]
  (etf-encoder/encode (bert-encoder/encode obj)))

(defn decode-seq [coll]
  (if (not (empty? coll))
    (let [[obj size] (etf-decoder/decode-with-size coll)]
      (cons obj (lazy-seq (decode-seq (drop size coll)))))))

(defn decode [coll]
  (first (decode-seq coll)))
