(ns aoc2017.day19
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines]]
            [aoc2017.util :refer [is-letter]]))
(def input (-> "day19.in" io/resource io/file slurp))

(defn parse-input [input]
  (->> input
    (split-lines)))

(defn nonspace-indices [coll]
  (keep-indexed
    (fn [idx val] (if (not= val \space) idx)) coll))

(defn find-start [grid]
  [(first (nonspace-indices (first grid))) 0])

(defn grid-get [grid [x y]]
  (or
    (get (get grid y) x)
    \space))

(defn next-directions
  " Given the last move, returns possible directions after turning left or right."
  [[dx dy]]
  (if (zero? dx)
    [[1 0] [-1 0]]
    [[0 1] [0 -1]]))

(defn change-dir [grid [x y] [dx dy]]
  (let [dirs (next-directions [dx dy])
        poss (map #(map + % [x y]) dirs)
        vals (map #(grid-get grid %) poss)
        idx (first (nonspace-indices vals))]
    (nth dirs idx)))

(defn next-dir
  "Change direction on +, return nil when done, keep straight otherwise."
  [grid pos dir]
  (let [val (grid-get grid pos)]
    (case val
      \+ (change-dir grid pos dir)
      \space nil
      dir)))

(defn solve [grid]
  (loop [pos (find-start grid) dir [0 1] letters "" counter 0]
    (let [val (grid-get grid pos)
          dir (next-dir grid pos dir)
          pos (map + pos dir)
          letters (if (is-letter val) (str letters val) letters)]
      (if (nil? dir)
        [letters counter]
        (recur pos dir letters (inc counter))))))

(defn main []
  (let [grid (parse-input input)
        [letters counter] (solve grid)]
    (println "Letters" letters)
    (println "Count" counter)))
