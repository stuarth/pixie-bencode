(ns pixie-bencode.read
  (:require [pixie.streams.utf8 :as stream :refer [read-char]]))

(defprotocol IPushbackInputStream
  (-unread [s n]))

(defrecord UTF8PushbackInputStream [input-stream buffer]
  stream/IUTF8InputStream
  (read-char [_]
    (let [buffer-v @buffer]
      (if (empty? buffer-v)
        (stream/read-char input-stream)
        (do
          (reset! buffer (next buffer-v))
          (first buffer-v)))))

  IPushbackInputStream
  (-unread [_ c]
    (swap! buffer conj c)
    nil))

(defn ->pushback-input-stream [input-stream]
  (->UTF8PushbackInputStream input-stream (atom [])))

(defn read-netstring
  [input-stream]
  (let [len (loop [len-str ""]
              (let [next-c (read-char input-stream)]
                (if (= \: next-c)
                  (read-string len-str)
                  (recur (str len-str next-c)))))]
    (loop [s ""
           c len]
      (if (zero? c)
        s
        (recur (str s (read-char input-stream))
               (dec c))))))

(defn read-token
  [input-stream]
  (let [c (read-char input-stream)]
    (condp = c
        \i :integer
        \l :list
        \d :map
        (do
          (-unread input-stream c)
          (read-netstring input-stream)))))

(defn read-integer
  [input-stream]
  (loop [int-str ""]
    (let [next-c (read-char input-stream)]
      (if (= \e next-c)
        (read-string int-str)
        (recur (str int-str next-c))))))

(defn read-list
  [input-stream]
  (loop [list []]
    (let [next-c (read-char input-stream)]
      (if (= \e next-c)
        list
        (do
          (-unread input-stream next-c)
          (recur (conj list (read-stream input-stream))))))))

(defn read-kv
  [input-stream]
  (let [k (read-netstring input-stream)
        v (read-stream input-stream)]
    [(keyword k) v]))

(defn read-map
  [input-stream]
  (loop [m {}]
    (let [next-c (read-char input-stream)]
      (if (= \e next-c)
        m
        (do
          (-unread input-stream next-c)
          (recur (conj m (read-kv input-stream))))))))

(defn read-stream
  [input-stream]
  (let [t (read-token input-stream)]
    (case t
      :integer (read-integer input-stream)
      :list    (read-list input-stream)
      :map     (read-map input-stream)
      t)))
