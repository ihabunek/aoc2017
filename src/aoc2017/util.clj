(ns aoc2017.util)

(defn parse-int [str]
  (Integer/parseInt str))

(defn abs [n]
  (max n (- n)))
