(ns aoc2017.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [aoc2017.util :refer [parse-int]]))

(def input "147,37,249,1,31,2,226,0,161,71,254,243,183,255,30,70")

(def input1
  (map parse-int
    (s/split input #",")))

(defn rotate
  "Rotate a circular coll for n positions. n can be negative."
  [coll n]
  (->> (cycle coll)
       (drop (mod n (count coll)))
       (take (count coll))))

(defn reverse-first
  "Reverse the order of the first n characters in coll."
  [coll n]
  (concat
    (reverse (take n coll))
    (drop n coll)))

(defn reverse-slice
  "Return the list with elements between (start, start + length) reversed in order. "
  [lst start length]
  (-> lst
      (rotate start)
      (reverse-first length)
      (rotate (- start))))

(defn solve1
  ([lst lengths] (solve1 lst lengths 0 0))
  ([lst lengths pos skip]
   (loop [lst lst lengths lengths pos pos skip skip]
     (let [len (first lengths)]
       (if (empty? lengths)
         (list lst pos skip)
         (recur
           (reverse-slice lst pos len)
           (rest lengths)
           (mod (+ pos len skip) (count lst))
           (mod (inc skip) (count lst))))))))

(defn solve2
  "Call solve1 64 times, carry over position and skip size."
  [lst lengths]
  (loop [lst lst pos 0 skip 0 counter 64]
    (if (zero? counter)
      lst
      (let [[lst pos skip] (solve1 lst lengths pos skip)]
        (recur lst pos skip (dec counter))))))

(defn condense [sparse-hash]
  (map #(apply bit-xor %)
    (partition 16 sparse-hash)))

(defn to-hex [dense-hash]
  (apply str
    (map #(format "%02x" %) dense-hash)))

(defn knot-hash-input [s]
  (concat
    (map int s)
    '(17 31 73 47 23)))

(defn knot-hash [input]
  (to-hex
    (condense
      (solve2 (range 256) (knot-hash-input input)))))

(defn main []
  (let [lst (range 256)
        [sol1 _ _] (solve1 lst input1)]
    (println "Sum of first two numbers is" (time (reduce * (take 2 sol1))))
    (println "Knot hex solution is" (time (knot-hash input)))))
