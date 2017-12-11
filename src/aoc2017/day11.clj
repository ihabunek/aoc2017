(ns aoc2017.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [aoc2017.util :refer [abs]]))

(def input-file (-> "day11.in" io/resource io/file))

(def directions
  {:n '(0 1)  :ne '(1 0)  :se '(1 -1)
   :s '(0 -1) :sw '(-1 0) :nw '(-1 1)})

(defn parse-input [input]
  (->> (s/split input #",")
       (map keyword)
       (map directions)))

(defn distance
  "Return the minimal number of moves from origin to given position."
  [[x y]]
  (if (pos? (* x y))
    (abs (+ x y))
    (max (abs x) (abs y))))

(defn walk
  "Walk through moves from origin, return end point and max distance traversed."
  [moves]
  (loop [moves moves pos '(0 0) max-dist 0]
    (if (empty? moves)
      [pos max-dist]
      (recur
        (rest moves)
        (map + pos (first moves))
        (max max-dist (distance pos))))))

(defn main []
  (let [moves (parse-input (slurp input-file))
        [end-pos max-dist] (walk moves)]
    (println "The end point is" (distance end-pos) "moves from the origin.")
    (println "The farthest point is" max-dist "moves from the origin.")))
