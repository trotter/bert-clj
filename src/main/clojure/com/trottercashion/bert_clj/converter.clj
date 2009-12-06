(ns com.trottercashion.bert-clj.converter)

(def *type-mappings*
  { nil               :nil
    java.lang.Boolean :boolean})

(def *type-finders*
  { clojure.lang.IPersistentMap :dictionary})

(defn conversion-type [obj]
  (or (*type-mappings* (type obj))
      (last (first (filter #(instance? (first %) obj) *type-finders*)))))

(defmulti convert #(conversion-type %))

(defmethod convert :nil [_]
  ['bert 'nil])

(defmethod convert :boolean [bool]
  (let [sym (if bool 'true 'false)]
    ['bert sym]))

(defmethod convert :dictionary [dict]
  (vector 'bert 'dict (map vec dict)))

