(ns aoc2017.day21
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines split join]]
            [clojure.pprint :refer [pprint]]
            [clojure.math.numeric-tower :refer [sqrt]]))

(def input (-> "day21.in" io/resource slurp))
(def example-input (-> "day21-example.in" io/resource slurp))

(def start '((\. \# \.)
             (\. \. \#)
             (\# \# \#)))

(defn parse-section [section]
  (map seq
    (split section #"/")))

(defn parse-line [line]
  (->> (split line #" => ")
       (mapv parse-section)))

(defn parse-input [input]
  (->> input
    (split-lines)
    (map parse-line)
    (into {})))

(defn fmt-grid [image]
  (join "\n" (map join image)))

(defn fmt-line [image]
  (join "/" (map join image)))

(defn rotate [image]
  (let [r (range (count image))]
    (for [col r]
      (for [row (reverse r)]
        (nth (nth image row) col)))))

(defn vflip [image]
  (reverse image))

(defn hflip [image]
  (map vflip image))

(defn variants [image]
  (concat
    (take 4 (iterate rotate image))
    (take 4 (iterate rotate (vflip image)))
    (take 4 (iterate rotate (hflip image)))))

(defn find-next [rules image]
  (loop [vs (variants image)]
    (if (empty? vs)
      (throw (Throwable. (str "Cannot find rule for " (fmt-line image)))))
    (if-let [rule (get rules (first vs))]
      rule
      (recur (rest vs)))))

(defn sub-image [image size x y]
  (map #(take size (drop x %))
    (take size (drop y image))))

(defn partition-image' [image part-size]
  (let [part-count (/ (count image) part-size)]
    (assert (integer? part-count))
    (for [p (range part-count)
          q (range part-count)]
      (sub-image image part-size (* p part-size) (* q part-size)))))

(defn partition? [image]
  (> (count image) 3))

(defn partition-size [image]
  (if (zero? (mod (count image) 2))
    2 3))

(defn partition-image [image]
  (if (partition? image)
    (partition-image' image (partition-size image))
    [image]))

(defn join-images [images]
  (let [n (sqrt (count (flatten images)))]
    (->> images
         (apply concat)
         (partition n)
         (apply interleave)
         (apply concat)
         (partition n))))

(defn count-pixels [image]
  (count
    (filter #(= \# %)
      (flatten image))))

(defn solve [start-image rules limit]
  (let [find-next (memoize (partial find-next rules))]
    (loop [image start-image
           counter limit]
      (if (zero? counter)
        (count-pixels image)
        (let [images (partition-image image)
              next-images (map find-next images)
              next-image (join-images next-images)]
          (recur next-image (dec counter)))))))

(defn main []
  (let [rules (parse-input input)]
    (println "Pixels after 5 iterations:" (time (solve start rules 5)))
    ; "Elapsed time: 7.642709 msecs"
    (println "Pixels after 18 iterations:" (time (solve start rules 18)))))
    ; "Elapsed time: 108308.088678 msecs"))
