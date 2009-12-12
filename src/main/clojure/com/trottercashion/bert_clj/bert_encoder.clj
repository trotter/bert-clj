(ns com.trottercashion.bert-clj.bert-encoder)

(def *type-mappings*
  { nil                     :nil
    java.lang.Boolean       :boolean
    java.util.regex.Pattern :regex})

(def *type-finders*
  { clojure.lang.IPersistentMap :dictionary
    java.util.Date              :time})

(def *regex-flags*
  {
    java.util.regex.Pattern/CASE_INSENSITIVE 'caseless
    java.util.regex.Pattern/COMMENTS         'extended
    java.util.regex.Pattern/MULTILINE        'multiline
    java.util.regex.Pattern/DOTALL           'dotall
    java.util.regex.Pattern/UNICODE_CASE     'unicode
  })

(def *inverse-regex-flags*
  { java.util.regex.Pattern/UNIX_LINES       ['newline 'anycrlf] })

(defn encoding-type [obj]
  (or (*type-mappings* (type obj))
      (last (first (filter #(instance? (first %) obj) *type-finders*)))))

(defn extract-regex-pattern [regex]
  (last (re-matches #"^(\(\?[ixmsud-]+\))*(.*)" (.pattern regex))))

(defn extract-regex-options [regex]
  (let [flags (.flags regex)
        matches-flag? #(= (bit-and (key %) flags) (key %))]
    (remove nil? (concat (map #(if (matches-flag? %) (val %)) *regex-flags*)
                         (map #(if (matches-flag? %) nil (val %)) *inverse-regex-flags*)))))

(defmulti encode #(encoding-type %))

(defmethod encode :nil [_]
  ['bert 'nil])

(defmethod encode :boolean [bool]
  (let [sym (if bool 'true 'false)]
    ['bert sym]))

(defmethod encode :dictionary [dict]
  (vector 'bert 'dict (map vec dict)))

(defmethod encode :time [time]
  (let [milliseconds (.getTime time)
        seconds      (quot milliseconds 1000)
        megaseconds  (quot seconds 1000000)
        microseconds (* (rem milliseconds 1000) 1000)]
    (vector 'bert 'time megaseconds (rem seconds 1000000) microseconds)))

(defmethod encode :regex [regex]
  (vector 'bert 'regex (extract-regex-pattern regex) (extract-regex-options regex)))

(defmethod encode :default [obj] obj)