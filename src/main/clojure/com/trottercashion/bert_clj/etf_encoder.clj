(ns com.trottercashion.bert-clj.etf-encoder
  (:use com.trottercashion.bert-clj.utility)
  (:require [clojure.contrib.math :as math]
            [com.trottercashion.bert-clj.bert :as bert]))

(declare encode-without-magic)

(def *type-mappings*
  { java.lang.String                      :string
    java.lang.Double                      :float
    java.lang.Integer                     :integer
    clojure.lang.Symbol                   :atom
    clojure.lang.Keyword                  :atom
    clojure.lang.PersistentList           :list
    clojure.lang.LazySeq                  :list
    clojure.lang.LazilyPersistentVector   :tuple
    clojure.lang.PersistentVector         :tuple
    java.lang.Long                        :bignum
    java.math.BigInteger                  :bignum
    clojure.lang.PersistentList$EmptyList :nil})

(defn data->bytes [data]
  (if (= 0 data)
    nil
    (lazy-seq (cons (bit-and 255 data) (data->bytes (bit-shift-right data 8))))))

(defn extract-bytes [data length]
  (reverse (take length (concat (data->bytes data) (repeat 0)))))

(defn twoByteLength [bytes]
  (let [size (count bytes)]
    (extract-bytes size 2)))

(defn coerce [kind & args]
  (let [stuff (concat [*version* (*etf-types* kind)] (apply concat args))]
    (map byte stuff)))

(defn encode-list [coll]
  (let [size (count coll)]
    (coerce :list (extract-bytes size 4) (apply concat (map encode-without-magic coll)) (encode-without-magic '()))))

(defn encode-binary-list [coll]
  (let [size (count coll)]
    (coerce :binary (extract-bytes size 4) coll)))

(defmulti encode #(*type-mappings* (type %)))

(defmethod encode :string [string]
  (let [bytes (.getBytes string)]
    (coerce :string (twoByteLength bytes) bytes)))

(defmethod encode :float [f]
  (let [bytes (.getBytes (format "%.20e" f))]
    (coerce :float bytes)))

(defmethod encode :integer [i]
  (if (and (< 0 i) (< i 256))
    (coerce :small-int (extract-bytes i 1))
    (coerce :big-int (extract-bytes i 4))))

(defmethod encode :atom [sym]
  (let [bytes (.getBytes (name sym))]
    (coerce :atom (twoByteLength bytes) bytes)))

(defmethod encode :list [coll]
  (if (every? #(= java.lang.Byte (type %)) coll)
    (encode-binary-list coll)
    (encode-list coll)))

(defmethod encode :tuple [coll]
  (let [size (count coll)]
    (if (> size 255)
      (coerce :large-tuple (extract-bytes size 4) (apply concat (map encode-without-magic coll)))
      (coerce :small-tuple (extract-bytes size 1) (apply concat (map encode-without-magic coll))))))

(defmethod encode :nil [_]
  (coerce :nil))

(defmethod encode :bignum [i]
  (let [sign (if (< i 0) 1 0)
        ;; need the abs because data->bytes goes nuts otherwise
        bytes (data->bytes (math/abs i))
        size (count bytes)]
    (if (> size 255)
      (coerce :large-bignum (extract-bytes size 4) (extract-bytes sign 1) bytes)
      (coerce :small-bignum (extract-bytes size 1) (extract-bytes sign 1) bytes))))

(defn encode-without-magic [o]
  (rest (bert/encode o)))

