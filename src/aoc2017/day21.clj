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
  (let [r0 image
        r1 (rotate r0)
        r2 (rotate r1)
        r3 (rotate r2)
        vfr0 (vflip image)
        vfr1 (rotate vfr0)
        vfr2 (rotate vfr1)
        vfr3 (rotate vfr2)
        hfr0 (hflip image)
        hfr1 (rotate hfr0)
        hfr2 (rotate hfr1)
        hfr3 (rotate hfr2)]
    (set [r0 r1 r2 r3
          vfr0 vfr1 vfr2 vfr3
          hfr0 hfr1 hfr2 hfr3])))

(defn find-next [image rules]
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
  (loop [image start-image
         counter limit]
    (if (zero? counter)
      (count-pixels image)
      (let [images (partition-image image)
            next-images (map #(find-next % rules) images)
            next-image (join-images next-images)]
        (recur next-image (dec counter))))))

(defn main []
  (let [rules (parse-input input)]
    (println "Pixels after 5 iterations:" (time (solve start rules 5)))
    ; "Elapsed time: 30.785062 msecs"))
    (println "Pixels after 18 iterations:" (time (solve start rules 18)))))
    ; "Elapsed time: 152569.048427 msecs"
