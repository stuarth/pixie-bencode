(ns pixie-bencode.write
  (:require [pixie.streams.utf8 :as stream :refer [write-char]]))

(defprotocol IBencode
  (bencode [o]))

(extend-protocol BEncode
  Number
  (bencode [n] (str "i" n "e"))

  PersistentHashMap
  (bencode [m]
    (str "d"
         (reduce
          (fn [acc [k v]]
            (str acc (bencode k) (bencode v)))
          ""
          (seq m))
         "e"))

  PersistentVector
  (bencode [s]
    (str
     "l"
     (reduce
      (fn [acc o]
        (str acc (bencode o)))
      ""
      s)
     "e"))

  String
  (bencode [s]
    (str (count s) ":" s))

  Keyword
  (bencode [k]
    (bencode (name k))))
