(ns aoc2017.day03)

; http://adventofcode.com/2017/day/3

(def directions
  "Directions of movement, clockwise starting with up."
  (cycle '((1 0) (0 1) (-1 0) (0 -1))))


(def distances
  "Distances of movement: 1, 1, 2, 2, 3, 3, ..."
  (map inc
    (interleave (range) (range))))

(def movements
  "Movements through the grid."
  (->> (map list distances directions) ; join distances and directions
       (map #(apply repeat %))         ; repeat each `direction` `distance` times
       (apply concat)))                ; flatten

(defn coordinates
  "Sum up first n movements to get coordinates for nth position"
  [n]
  (apply map +
    (take (dec n) movements)))

(defn distance
  "Distance from center for given grid coordinates"
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))

; -- Second part --

(defn neighbours
  "For given position returns a list of neighbouring coordinates."
  [[x y]]
  (for [x1 [(dec x) x (inc x)]
        y1 [(dec y) y (inc y)]]
    (list x1 y1)))

(defn sum-neighbours
  [grid pos]
  (->> pos
       (neighbours)
       (map #(get grid % 0))
       (reduce +)))

(defn next-in-sequence [limit]
  (loop [grid {'(0 0) 1}
         pos '(0 0)
         value 1
         movs movements]
    (if (> value limit) value
      (let [next-pos (map + pos (first movs))
            next-value (sum-neighbours grid next-pos)
            next-grid (assoc grid next-pos next-value)]
        (recur next-grid next-pos next-value (rest movs))))))

(defn main []
  (println "\nFirst part")
  (println "coordinates:" (coordinates 312051))
  (println "distance:" (distance (coordinates 312051)))
  (println "\nSecond part")
  (println "next in sequence:" (next-in-sequence 312051)))
