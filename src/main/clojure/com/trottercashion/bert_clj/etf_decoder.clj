(ns com.trottercashion.bert-clj.etf-decoder
  (:use com.trottercashion.bert-clj.utility))

(defn unsign-int [i]
  (if (< i 0)
    (+ 256 i)
    i))

(defn make-byte-array [coll]
  (let [array (make-array Byte/TYPE (count coll))]
    (doseq [[idx val] (zipmap (iterate inc 0) coll)]
      (aset array idx (byte val)))
    array))

(defn bytes->data [data]
  (reduce #(+ (bit-shift-left %1 8) (unsign-int %2)) 0 data))

(defmacro defdecoder [type & body]
  `(defmethod decode ~type [coll#]
     (let [[magic# _# & data#] coll#
           bodyfun# (fn ~(first body) ~@(rest body))]
       (if (not (= magic# -125)) (throw "Unknown magic bit: we only handle 131 (-125 when signed)"))
       (bodyfun# data#))))

(defmulti decode
  (fn [coll]
    (let [[magic type data] coll]
      (*codes->etf-types* (int type)))))

(defdecoder :string [data]
  (let [length (bytes->data (take 2 data))
        bytes  (take length (drop 2 data))]
    (String. (make-byte-array bytes))))

(defdecoder :float [data]
  (Float. (String. (make-byte-array (take 26 data)))))

(defdecoder :small-int [data]
  (bytes->data (take 1 data)))

(defdecoder :big-int [data]
  (bytes->data (take 4 data)))
