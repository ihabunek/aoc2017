(ns aoc2017.day14
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join]]
            [clojure.pprint :refer [pprint cl-format]]
            [aoc2017.day10 :refer [knot-hash]]
            [aoc2017.day14-data :refer [cached-grid]]))

(defn hex-to-bin [c]
  (cl-format nil "~4'0B"
    (Integer/parseInt (str c) 16)))

(defn count-ones [s]
  (count
    (filter #(= \1 %) s)))

(defn build-grid [input]
  (for [row (range 128)
        :let [hash-input (str input \- row)
              hex-hash (knot-hash (map int hash-input))]]
    (join (map hex-to-bin hex-hash))))

(defn count-used-squares [grid]
  (reduce +
    (map count-ones grid)))

(defn grid-to-map
  "Converts the grid (list of strings) into a map indexed by position [x y] and
   having the value true if used, false if not."
  [grid]
  (into {}
    (for [x (range (count (first grid)))
          y (range (count grid))]
      [[x y] (not= \0 (get (get grid y) x))])))

(defn neighbours [[x y]]
  [[(inc x) y] [x (inc y)]
   [(dec x) y] [x (dec y)]])

(defn used-neighbours [grid-map pos]
  (filter #(get grid-map %) (neighbours pos)))

(defn find-start
  "Locates the first used position on the grid, or nil if none found."
  [grid-map]
  (first (first (filter val grid-map))))

(defn find-region [grid-map pos]
  (assert (not (nil? (get grid-map pos))))
  (let [neighbours (used-neighbours grid-map pos)]
    ; (println "find-region" pos neighbours)
    (if (empty? neighbours)
      [pos]
      (concat [pos]
        (mapcat #(find-region (assoc! grid-map pos false) %) neighbours)))))

(defn clear-region [grid-map positions]
  (loop [grid-map grid-map positions positions]
    (if (empty? positions)
      grid-map
      (recur
        (assoc grid-map (first positions) false)
        (rest positions)))))

(defn count-regions [grid]
  (loop [grid-map (grid-to-map grid)
         region-count 0]
    (let [pos (find-start grid-map)]
      (if (nil? pos)
        region-count
        (let [region (find-region (transient grid-map) pos)]
          (recur
            (clear-region grid-map region)
            (inc region-count)))))))

(defn main []
  (let [grid cached-grid]
    (println "Part 1: There are" (time (count-used-squares grid)) "used squares")
    (println "Part 2: Found" (time (count-regions grid)) "regions")))
