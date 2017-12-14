(ns aoc2017.day13
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split split-lines]]
            [clojure.pprint :refer [pprint]]
            [aoc2017.util :refer [parse-int]]))

(def input-file (-> "day13.in" io/resource io/file))

(defn parse-line [line]
  (->> (split line #": ")
       (mapv parse-int)))

(defn parse-input [input]
  (->> input
      (split-lines)
      (map parse-line)
      (into {})))

(defn caught? [time depth]
  (and
    (not (nil? depth))
    (zero? (mod time (* (dec depth) 2)))))

(defn severity [time depth]
  (if (caught? time depth)
    (* time depth) 0))

(defn severities [depths]
  (reduce + (map-indexed severity depths)))

(defn not-caught [depths delay]
  (loop [depths depths delay delay]
    (cond
      (empty? depths) true
      (caught? delay (first depths)) false
      :else (recur (rest depths) (inc delay)))))

(defn find-uncaught-delay [depths]
  (loop [delay 0]
    (if (not-caught depths delay)
      delay
      (recur (inc delay)))))

(defn main []
  (let [depths-map (parse-input (slurp input-file))
        len (inc (apply max (keys depths-map)))
        depths (map depths-map (range len))]
    (println "Severity with no delay:" (time (severities depths)))
    (println "Smallest delay without getting caught:"
      (time (find-uncaught-delay depths)))))
