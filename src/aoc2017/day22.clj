(ns aoc2017.day22
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines split join]]
            [clojure.pprint :refer [pprint]]))

(def input (-> "day22.in" io/resource slurp))

(def example-input (-> "day22-example.in" io/resource slurp))

(def directions  {:up [0 -1], :down [0 1],  :left [-1 0], :right [1 0]})
(def turn-right  {:up :right, :down :left,  :left :up,    :right :down})
(def turn-left   {:up :left,  :down :right, :left :down,  :right :up})
(def turn-around {:up :down,  :down :up,    :left :right, :right :left})

(defn parse-input [input]
  (set
    (let [lines (split-lines input)
          height (count lines)
          width (count (first lines))
          ; offset to make the center position at [0 0]
          dx (int (/ height 2))
          dy (int (/ width 2))]
      (for [x (range width)
            y (range height)
            :let [char (nth (nth lines y) x)]
            :when (= char \#)]
        [(- x dx) (- y dy)]))))

(defn solve1 [grid limit]
  (loop [grid grid pos [0 0] dir :up infected-count 0 counter 0]
    (if (= counter limit)
      infected-count
      (let [infected? (contains? grid pos)
            dir (if infected? (turn-right dir) (turn-left dir))
            grid (if infected? (disj grid pos) (conj grid pos))
            pos (mapv + pos (directions dir))
            infected-count (+ infected-count (if infected? 0 1))]
        (recur grid pos dir infected-count (inc counter))))))

(def transition
  {:clean :weakened
   :weakened :infected
   :infected :flagged
   :flagged :clean})

(defn turn [dir state]
  (case state
    :clean (turn-left dir)
    :weakened dir
    :infected (turn-right dir)
    :flagged (turn-around dir)))

(defn set->map [grid]
  (into {} (for [pos grid] [pos :infected])))

(defn solve2 [grid limit]
  (loop [grid (set->map grid) pos [0 0] dir :up infected-count 0 counter 0]
    (if (= counter limit)
      infected-count
      (let [state (get grid pos :clean)
            next-state (transition state)
            next-grid (assoc grid pos next-state)
            next-dir (turn dir state)
            next-pos (mapv + pos (directions next-dir))
            infected-count (+ infected-count (if (= next-state :infected) 1 0))]
        (recur next-grid next-pos next-dir infected-count (inc counter))))))

(defn main []
  (let [grid (parse-input input)]
    (pprint (time (solve1 grid 10000)))
    ; "Elapsed time: 27.533392 msecs"
    (pprint (time (solve2 grid 10000000)))))
    ; "Elapsed time: 25223.689071 msecs"
