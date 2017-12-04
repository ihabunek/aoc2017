(ns aoc2017.day04
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split split-lines]]))

(def input-file (-> "day04.in" io/resource io/file))

(defn parse-input
  "Split input into lines, split each line into words."
  [input]
  (->> input
       split-lines
       (map #(split % #" "))))

(defn max-occurence-count
  "Return the maximum number of times any of the words is repeated."
  [words]
  (->> words
       frequencies
       vals
       (apply max)))

(defn valid? [words]
  (= 1 (max-occurence-count words)))

(defn count-valid [passwords]
  (count
    (filter valid? passwords)))

(defn main []
  (let [passwords (parse-input (slurp input-file))]
    (println "Part 1, valid passwords:" (count-valid passwords))
    ; To get second part, sort characters in each word before checking validity
    (let [sorted-passwords (map #(map sort %) passwords)]
      (println "Part 2, valid passwords:" (count-valid sorted-passwords)))))
