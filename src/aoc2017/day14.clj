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

(defn grid-to-set [grid]
  (into #{}
    (for [x (range (count (first grid)))
          y (range (count grid))
          :when (not= \0 (get (get grid y) x))]
      [x y])))

(defn neighbours [[x y]]
  [[(inc x) y] [x (inc y)]
   [(dec x) y] [x (dec y)]])

(defn used-neighbours [grid-set pos]
  (filter #(contains? grid-set %) (neighbours pos)))

(defn find-region [grid-set pos]
  (let [neighbours (used-neighbours grid-set pos)]
    (if (empty? neighbours)
      [pos]
      (concat [pos]
        (mapcat #(find-region (disj! grid-set pos) %) neighbours)))))

(defn count-regions [grid]
  (loop [grid-set (grid-to-set grid)
         region-count 0]
    (let [pos (first grid-set)]
      (if (nil? pos)
        region-count
        (let [region (find-region (transient grid-set) pos)]
          (recur
            (apply disj grid-set region)
            (inc region-count)))))))

(defn main []
  (let [grid cached-grid]
    ; (println "Part 1: There are" (time (count-used-squares grid)) "used squares")
    (println "Part 2: Found" (time (count-regions grid)) "regions")))
