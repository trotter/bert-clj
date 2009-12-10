(ns com.trottercashion.bert-clj.etf-decoder
  (:use com.trottercashion.bert-clj.utility))

(defn make-byte-array [coll]
  (let [array (make-array Byte/TYPE (count coll))]
    (doseq [[idx val] (zipmap (iterate inc 0) coll)]
      (aset array idx (byte val)))
    array))

(defn bytes->data [data]
  (reduce #(+ (bit-shift-left %1 8) %2) data))

(defmulti decode
  (fn [coll]
    (let [[magic type data] coll]
      (*codes->etf-types* (int type)))))

(defmethod decode :string [coll]
  (let [[magic type & data] coll
         length           (bytes->data (take 2 data))
         bytes            (take length (drop 2 data))]
    (String. (make-byte-array bytes))))

(defmethod decode :float [coll]
  (let [[magic type & data] coll]
    (Float. (String. (make-byte-array (take 26 data))))))