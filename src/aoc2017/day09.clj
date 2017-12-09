(ns aoc2017.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input-file (-> "day09.in" io/resource io/file))

(defn isolate-groups
  "Removes garbage and converts the remaining groups to clojure lists."
  [input]
  (-> input
      (s/replace #"!." "")      ; Remove canceled characters
      (s/replace #"<.*?>" "")   ; Remove garbage (ungreedy)
      (s/replace "," "")        ; Remove commas
      (s/replace "{" "(")       ; Convert braces to brackets
      (s/replace "}" ")")       ; Convert braces to brackets
      (read-string)))           ; Convert to a clojure list

(defn count-garbage [input]
  (->> (s/replace input #"!." "")  ; Remove canceled characters
       (re-seq #"<(.*?)>")         ; Find garbage groups
       (map second)                ; Isolate just the matched group
       (apply concat)              ; Join groups together
       (count)))                   ; Count characters

(defn calc-score
  ([groups] (calc-score groups 1))
  ([groups depth]
   (if (empty? groups) depth
     (+ depth
       (reduce +
         (map #(calc-score % (inc depth)) groups))))))

(defn main []
  (let [input (slurp input-file)]
    (println "Score:" (calc-score (isolate-groups input)))
    (println "Garbage character count:" (count-garbage input))))
