(ns com.trottercashion.bert-clj.encoder)

(declare encode-without-magic)

(def *version* (byte 131))

(def *etf-types* 
  { :small-int 97
    :big-int   98
    :float     99
    :atom      100
    :nil       106
    :string    107
    :list      108
    :binary    109})

(def *type-mappings*
  { java.lang.String            :string
   java.lang.Double            :float
   java.lang.Integer           :integer
   clojure.lang.Symbol         :atom
   clojure.lang.PersistentList :list
   clojure.lang.LazySeq        :list})

(defn data->bytes [data]
  (lazy-seq (cons (bit-and 255 data) (data->bytes (bit-shift-right data 8)))))

(defn extract-bytes [data length]
  (reverse (take length (data->bytes data))))

(defn twoByteLength [bytes]
  (let [size (count bytes)]
    (extract-bytes size 2)))

(defn coerce [kind & args]
  (let [stuff (concat [*version* (*etf-types* kind)] (apply concat args))]
    (map byte stuff)))

(defn encode-list [coll]
  (let [size (count coll)]
    (coerce :list (extract-bytes size 4) (apply concat (map encode-without-magic coll)) (encode-without-magic nil))))

(defn encode-binary-list [coll]
  (let [size (count coll)]
    (coerce :binary (extract-bytes size 4) coll)))

(defmulti encode #(*type-mappings* (type %)))

(defmethod encode :string [string]
  (let [bytes (.getBytes string)]
    (coerce :string (twoByteLength bytes) bytes)))

(defmethod encode :float [f]
  (let [bytes (.getBytes (format "%.20e" 5.5))]
    (coerce :float bytes)))

(defmethod encode :integer [i]
  (if (< i 256)
    (coerce :small-int (extract-bytes i 1))
    (coerce :big-int (extract-bytes i 4))))

(defmethod encode :atom [sym]
  (let [bytes (.getBytes (str sym))]
    (coerce :atom (twoByteLength bytes) bytes)))

(defmethod encode :list [coll]
  (if (every? #(= java.lang.Byte (type %)) coll)
    (encode-binary-list coll)
    (encode-list coll)))

(defmethod encode nil [_]
  (coerce :nil))

(defn encode-without-magic [o]
  (rest (encode o)))

