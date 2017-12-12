(ns aoc2017.day12
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split split-lines]]
            [clojure.set :refer [union difference]]
            [clojure.pprint :refer [pprint]]
            [aoc2017.util :refer [parse-int]]))

(def input-file (-> "day12.in" io/resource io/file))

(defn parse-line [line]
  (let [[left right] (split line #" <-> ")]
    [(parse-int left)
     (->> (split right #", ")
          (map parse-int)
          (set))]))

(defn parse-input [input]
  (->> (split-lines input)
       (map parse-line)
       (into {})))

(defn find-group
   "For a given starting program returns all programs connected to it."
   ([prog-map start]
    (find-group prog-map start #{}))
   ([prog-map program group]
    (let [all-neighbours (prog-map program)
          new-neighbours (difference all-neighbours group) ; neighbours not yet in group
          next-group (union group new-neighbours)]
      (if (empty? new-neighbours)
        group
        (apply union
          (map #(find-group prog-map % next-group) new-neighbours))))))

(defn count-groups [prog-map]
  (loop [prog-map prog-map groups 0]
    (if (empty? prog-map)
      groups
      (let [program (first (keys prog-map))
            group (find-group prog-map program)
            next-prog-map (apply dissoc prog-map group)] ; remove processed programs
        (recur next-prog-map (inc groups))))))

(defn main []
  (let [prog-map (parse-input (slurp input-file))
        solution1 (count (find-group prog-map 0))
        solution2 (count-groups prog-map)]
    (println "There are" solution1 "programs in the group with program 0.")
    (println "There are" solution2 "groups in total")
    (println "QED")))
