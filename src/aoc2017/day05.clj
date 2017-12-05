(ns aoc2017.day05
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split split-lines]]))

(def input-file (-> "day05.in" io/resource io/file))

(defn parse-input [input]
  (->> input
       split-lines
       (mapv read-string)))

(defn solve1 [moves]
  (loop [moves (transient moves) position 0 counter 1]
    (let [move (get moves position)
          next-position (+ position move)]
      (if (nil? (get moves next-position))
        counter
        (recur
          (assoc! moves position (inc move))
          next-position
          (inc counter))))))

(defn solve2 [moves]
  (loop [moves (transient moves) position 0 counter 1]
    (let [move (get moves position)
          next-position (+ position move)]
      (if (nil? (get moves next-position))
        counter
        (recur
          (assoc! moves position
            (if (> move 2) (dec move) (inc move)))
          next-position
          (inc counter))))))

(defn main []
  (let [moves (parse-input (slurp input-file))]
    (println "First part solved in" (time (solve1 moves)) "steps")
    (println "Second part solved in" (time (solve2 moves)) "steps")))
