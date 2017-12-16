(ns aoc2017.day16
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split join]]
            [clojure.pprint :refer [pprint]]
            [aoc2017.util :refer [parse-int index-of]]))


(def input (-> "day16.in" io/resource io/file slurp))

(defn parse-line [line]
  (let [move (keyword (subs line 0 1))
        rest (subs line 1)]
    (list move (case move
                 :p (->> (split rest #"/") (map keyword))
                 :s (list (parse-int rest))
                 :x (->> (split rest #"/") (map parse-int))))))

(defn parse-input [input]
  (->> (split input #",")
       (map parse-line)))

(defn spin [coll n]
  (->> (cycle coll)
       (drop (mod (- n) (count coll)))
       (take (count coll))
       (into [])))

(defn exchange [coll x y]
  (let [l (coll x) r (coll y)]
    (-> coll
      (assoc x r)
      (assoc y l))))

(defn partner [coll x y]
  (exchange coll
    (index-of coll x)
    (index-of coll y)))

(defn make-programs [n]
  (->> (range 97 (+ 97 n))
    (mapv char)
    (mapv str)
    (mapv keyword)))

(defn apply-move [programs [move args]]
  (case move
     :p (apply partner programs args)
     :s (apply spin programs args)
     :x (apply exchange programs args)))

(defn apply-moves [programs moves]
  (loop [programs programs moves moves]
    (if (empty? moves)
      programs
      (recur
        (apply-move programs (first moves))
        (rest moves)))))

(defn find-cycle [programs moves]
  (loop [programs programs cycle []]
    (if (some #{programs} cycle)
      cycle
      (recur
        (apply-moves programs moves)
        (conj cycle programs)))))

(defn dump [programs]
  (->> programs (map name) join))

(defn iterate [programs moves n]
  (let [cycle (find-cycle programs moves)
        index (mod n (count cycle))]
    (cycle index)))

(defn main []
  (let [moves (parse-input input)
        programs (make-programs 16)]
    ; "Elapsed time: 100.728047 msecs"
    (println "After the first dance positions are:"
      (dump (time (apply-moves programs moves))))
    ; "Elapsed time: 2790.029917 msecs"
    (println "One billionth iteration is:"
      (dump (time (iterate programs moves 1000000000))))))
