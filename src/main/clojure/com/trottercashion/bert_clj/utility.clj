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