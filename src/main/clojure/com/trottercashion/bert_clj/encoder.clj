(ns com.trottercashion.bert-clj.encoder)

(def *version* (byte 131))

(def *types* 
  { java.lang.String (byte 107)
    java.lang.Double (byte 99)
    :small-int       (byte 97)})

(defn bytes [& args]
  (map #(.toByte %) args))

(defn twoByteLength [bytes]
  (let [size (count bytes)
        second-byte (byte (bit-and 255 size))
        first-byte (byte (bit-shift-right size 8))]
    [first-byte second-byte]))

(defn coerce [kind & args]
  (concat [*version* (*types* kind)] (apply concat args)))

(defmulti encode #(type %))

(defmethod encode java.lang.String [string]
  (let [bytes (.getBytes string)]
    (coerce java.lang.String (twoByteLength bytes) bytes)))

(defmethod encode java.lang.Double [f]
  (let [bytes (.getBytes (format "%.20e" 5.5))]
    (coerce java.lang.Double bytes)))

(defmethod encode java.lang.Integer [i]
  (if (< i 256)
    (coerce :small-int [(byte i)])))


