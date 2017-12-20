(ns aoc2017.day20
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines]]
            [clojure.pprint :refer [pprint]]
            [aoc2017.util :refer [abs]]))

(def input (-> "day20.in" io/resource slurp))

(def pattern #"^p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>")

(defn weight [pos]
  (->> pos (map abs) (reduce +)))

(defn add-index [idx m]
  (assoc m :idx idx))

(defn parse-line [idx line]
  (->> (re-find pattern line)
       (rest) ; remove full match, leave only groups
       (map read-string)
       (partition 3)
       (zipmap [:pos :vel :acc])
       (add-index idx)))

(defn parse-input [input]
  (->> input
    (split-lines)
    (map-indexed parse-line)))

(defn keyfn [particle]
  (mapv weight
    ((juxt :acc :vel :pos) particle)))

(defn solve1 [particles]
  (:idx (first (sort-by keyfn particles))))

(defn step [particle]
  (let [particle (assoc particle :vel (map + (:vel particle) (:acc particle)))
        particle (assoc particle :pos (map + (:pos particle) (:vel particle)))]
    particle))

(defn find-collided [particles]
  (set
    (apply concat
      (for [group (group-by :pos particles)
            :let [[pos items] group]
            :when (> (count items) 1)]
        (map :idx items)))))

(defn remove-collided [particles]
  (let [indices (find-collided particles)]
    ; (if (not (empty? indices)) (println "Removing" indices))
    (filter #(not (contains? indices (:idx %))) particles)))

; This is not a very good solution
; Experimenting shows that after 100 iterations all collisions are done for
; given input, but this may not work on others. Should find a way to test if
; any more collisions are possible.
(defn solve2 [particles]
  (loop [particles particles counter 100]
    (let [particles (map step particles)
          particles (remove-collided particles)]
      (if (zero? counter)
        (count particles)
        (recur particles (dec counter))))))

(defn main []
  (let [particles (parse-input input)]
    (println "Closest particle:" (time (solve1 particles)))
    (println "Number of collisions:" (time (solve2 particles)))))
