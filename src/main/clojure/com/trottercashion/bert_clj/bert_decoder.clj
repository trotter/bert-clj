(ns com.trottercashion.bert-clj.bert-decoder)

(defmulti decoder (fn [type data] type))

(defmethod decoder 'nil   [type data] nil)
(defmethod decoder 'true  [type data] true)
(defmethod decoder 'false [type data] false)

(defmethod decoder 'dict [type [pairs]]
  (zipmap (map first pairs) (map second pairs)))

(defmethod decoder 'time [type data]
  (let [[mega seconds micro] data]
    (java.util.Date. (+ (* mega 1000000000)
                        (* seconds 1000)
                        (/ micro 1000)))))

(defn decode [coll]
  (let [[magic type & data] coll]
    (if (= magic 'bert)
      (decoder type data)
      (throw (str "Tuple is not a bert, magic:" magic)))))

