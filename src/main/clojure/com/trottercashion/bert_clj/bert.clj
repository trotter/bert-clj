(ns com.trottercashion.bert-clj.bert
  (:require [com.trottercashion.bert-clj.etf-encoder  :as etf-encoder]
            [com.trottercashion.bert-clj.bert-encoder :as bert-encoder]
            [com.trottercashion.bert-clj.etf-decoder  :as etf-decoder]
            [com.trottercashion.bert-clj.bert-decoder :as bert-decoder]))

(defn encode [obj]
  (etf-encoder/encode (bert-encoder/encode obj)))

(defn decode [coll]
  (let [etf-decoded (etf-decoder/decode coll)]
    (try
     (bert-decoder/decode etf-decoded)
     (catch Exception _ etf-decoded))))
