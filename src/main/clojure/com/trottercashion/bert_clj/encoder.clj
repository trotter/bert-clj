(ns com.trottercashion.bert-clj.encoder)

(def *version* (byte 131))

(def *types* 
  { :small-int 97
    :big-int   98
    :float     99
    :atom      100
    :string    107})

(defn data->bytes [data]
  (lazy-seq (cons (bit-and 255 data) (data->bytes (bit-shift-right data 8)))))

(defn extract-bytes [data length]
  (reverse (take length (data->bytes data))))

(defn twoByteLength [bytes]
  (let [size (count bytes)]
    (extract-bytes size 2)))

(defn coerce [kind & args]
  (map byte (concat [*version* (*types* kind)] (apply concat args))))

(defmulti encode #(type %))

(defmethod encode java.lang.String [string]
  (let [bytes (.getBytes string)]
    (coerce :string (twoByteLength bytes) bytes)))

(defmethod encode java.lang.Double [f]
  (let [bytes (.getBytes (format "%.20e" 5.5))]
    (coerce :float bytes)))

(defmethod encode java.lang.Integer [i]
  (if (< i 256)
    (coerce :small-int (extract-bytes i 1))
    (coerce :big-int (extract-bytes i 4))))

(defmethod encode clojure.lang.Symbol [sym]
  (let [bytes (.getBytes (str sym))]
    (coerce :atom (twoByteLength bytes) bytes)))

(defmethod encode :default [bytes]
  (coerce :string (twoByteLength bytes) bytes))

