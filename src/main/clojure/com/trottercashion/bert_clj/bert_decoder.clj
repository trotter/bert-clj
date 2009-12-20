(ns com.trottercashion.bert-clj.bert-decoder
  (:use com.trottercashion.bert-clj.utility)
  (:require [clojure.contrib.seq-utils :as seq-utils]))

(defmulti decoder (fn [type data] type))

(defmethod decoder :nil   [type data] nil)
(defmethod decoder :true  [type data] true)
(defmethod decoder :false [type data] false)

(defmethod decoder :dict [type [pairs]]
  (zipmap (map first pairs) (map second pairs)))

(defmethod decoder :time [type [mega seconds micro]]
  (java.util.Date. (+ (* mega 1000000000)
                      (* seconds 1000)
                      (/ micro 1000))))

(defn regex-flags [options]
  (let [inverse-options (remove nil? (map (fn [[key val]]
                                           (if (seq-utils/find-first #(= key %) options) nil val))
                                         *symbols->inverse-regex-flags*))]
    (reduce bit-or (concat (map #(*symbols->regex-flags* %) options)
                           inverse-options))))

(defmethod decoder :regex [type [source options]]
  (java.util.regex.Pattern/compile source (or (regex-flags options) 0)))

(defn decode [coll]
  (let [[magic type & data] coll]
    (if (= magic :bert)
      (decoder type data)
      coll)))

