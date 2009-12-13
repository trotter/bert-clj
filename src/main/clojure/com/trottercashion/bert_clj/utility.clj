(ns com.trottercashion.bert-clj.utility)

(def *version* (byte 131))

(def *etf-types*
  { :small-int    97
    :big-int      98
    :float        99
    :atom         100
    :small-tuple  104
    :large-tuple  105
    :nil          106
    :string       107
    :list         108
    :binary       109
    :small-bignum 110
    :large-bignum 111})

(def *codes->etf-types*
  (zipmap (vals *etf-types*) (keys *etf-types*)))

(def *regex-flags*
  {
    java.util.regex.Pattern/CASE_INSENSITIVE 'caseless
    java.util.regex.Pattern/COMMENTS         'extended
    java.util.regex.Pattern/MULTILINE        'multiline
    java.util.regex.Pattern/DOTALL           'dotall
    java.util.regex.Pattern/UNICODE_CASE     'unicode
  })

(def *symbols->regex-flags*
     (zipmap (vals *regex-flags*) (keys *regex-flags*)))

(def *inverse-regex-flags*
  { java.util.regex.Pattern/UNIX_LINES       ['newline 'anycrlf] })

(def *symbols->inverse-regex-flags*
     (zipmap (vals *inverse-regex-flags*) (keys *inverse-regex-flags*)))
