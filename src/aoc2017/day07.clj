(ns aoc2017.day07
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join split split-lines]]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [difference]]))

(def input-file (-> "day07.in" io/resource io/file))

(defn parse-line [line]
  (let [pattern #"^(\w+) \((\d+)\)( -> (.+))?"
        matches (re-find pattern line)]
    { :name (matches 1)
      :weight (Integer/parseInt (matches 2))
      :children (if (matches 4)
                  (split (matches 4) #", ") [])}))

(defn parse-input [input]
  (->> input
       split-lines
       (mapv parse-line)))

(defn get-bottom [programs]
  (let [all-names (set (map :name programs))
        child-names (set (mapcat :children programs))]
    (first
      (difference all-names child-names))))

(defn build-weight-map [programs]
  (into {}
    (mapv #(vector (:name %) (:weight %)) programs)))

(defn build-child-map [programs]
  (into {}
    (mapv #(vector (:name %) (:children %)) programs)))

(defn report-imbalance [node children weights child-weights]
  (let [freqs (frequencies child-weights)
        correct-stack-weight (first (first freqs))
        wrong-stack-weight (first (second freqs))
        wrong-index (second (second freqs))
        weight-diff (- wrong-stack-weight correct-stack-weight)
        wrong-node (nth children wrong-index)
        wrong-node-weight (weights wrong-node)
        correct-node-weight (- wrong-node-weight weight-diff)]
    (println (join (repeat 80 "=")))
    (println "Imbalance found at node:" node)
    (println "Children:" children)
    (println "Child weights:" child-weights)
    (println "Correct stack weight:" correct-stack-weight)
    (println "Wrong stack weight:" wrong-stack-weight)
    (println "Weight diff:" weight-diff)
    (println "Wrong node:" wrong-node)
    (println "Wrong node weight:" wrong-node-weight)
    (println "Correct node weight:" correct-node-weight)))

(defn total-weight
  "Calculates the total weight of the tree and prints out any imbalances"
  [child-map weights node]
  (let [children (get child-map node)]
    (if (empty? children)
      (weights node)
      (let [child-weights (map #(total-weight child-map weights %) children)]
        ; imbalance is found when not all child weights are equal
        (when (not (apply = child-weights))
          (report-imbalance node children weights child-weights))
        (+ (weights node) (reduce + child-weights))))))

(defn main []
  (let [programs (parse-input (slurp input-file))
        bottom (get-bottom programs)
        child-map (build-child-map programs)
        weights (build-weight-map programs)]
    (println "The bottom program is" bottom)
    (println "Weight of the tree is" (total-weight child-map weights bottom))))
