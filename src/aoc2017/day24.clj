(ns aoc2017.day24
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines split join]]
            [clojure.pprint :refer [pprint]]))

(def input (-> "day24.in" io/resource slurp))

(defn parse-input [input]
  (->> (split-lines input)
       (map #(split % #"/"))
       (map #(map read-string %))
       (map set)))

(defn drop-nth [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

(defn index-of [coll el]
  (first (keep-indexed #(if (= el %2) %1) coll)))

(defn proc-next [component components n]
  (let [index (index-of components component)
        rest (drop-nth index components)
        component (disj component n)
        next [n (if (empty? component) n (first component))]]
    [next rest]))

(defn get-next-rest [components n]
  (let [next-components (filter #(some #{n} %) components)]
    (map #(proc-next % components n) next-components)))

(defn strength [bridge]
  (reduce + (map (partial apply +) bridge)))

(defn build-bridges [components n bridge]
  (let [next-rests (get-next-rest components n)]
    (if (empty? next-rests)
      (list {:str (strength bridge) :len (count bridge)})
      (apply concat
        (map
          (fn [[next rest]]
            (build-bridges
              rest
              (second next)
              (conj bridge next)))
          next-rests)))))

(defn main []
  (let [components (parse-input input)
        bridges (time (build-bridges components 0 []))
        max-length (apply max (map :len bridges))
        max-strength (apply max (map :str bridges))
        longest-bridges (filter #(= max-length (:len %)) bridges)
        strongest-longest-bridge (apply max (map :str longest-bridges))]
    (println "Longest bridge:" max-length)
    (println "Strongest bridge:" max-strength)
    (println "Strongest longest bridge:" strongest-longest-bridge)))
