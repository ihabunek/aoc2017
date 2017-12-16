(ns aoc2017.util)

(defn parse-int [str]
  (Integer/parseInt str))

(defn abs [n]
  (max n (- n)))

(defn index-of [coll e]
  (first (keep-indexed #(if (= e %2) %1) coll)))
