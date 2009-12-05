(ns com.trottercashion.bert-clj.converter)

(def *type-mappings*
  { nil               :nil
    java.lang.Boolean :boolean})

(defmulti convert #(*type-mappings* (type %)))

(defmethod convert :nil [_]
  {'bert, 'nil})

(defmethod convert :boolean [bool]
  (let [sym (if bool 'true 'false)]
    {'bert, sym}))