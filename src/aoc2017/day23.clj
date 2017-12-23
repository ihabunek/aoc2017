(ns aoc2017.day23
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split join split-lines]]
            [clojure.pprint :refer [pprint]]
            [aoc2017.util :refer [parse-int]]))

(def input-file (-> "day23.in" io/resource io/file))

(defn parse-arg [arg]
  (let [arg (read-string arg)]
    (if (symbol? arg)
      (keyword arg) arg)))

(defn parse-command [string]
  (->> (split string #" ")
       (map parse-arg)))

(defn parse-input [input]
  (->> input
    (split-lines)
    (mapv parse-command)))

; macro candidate
(defn v [regs key]
  (if (keyword? key) (regs key 0) key))

(defn -set [regs pos key val]
  [(assoc regs key (v regs val))
   (inc pos)])

(defn generic [regs pos key val fun]
  (let [val (v regs val)]
    [(assoc regs key (fun (regs key 0) val))
     (inc pos)]))

(defn -sub [regs pos key val]
  (generic regs pos key val -))

(defn -mul [regs pos key val]
  (generic regs pos key val *))

(defn -jnz [regs pos key val]
  [regs (if (not (zero? (v regs key)))
          (+ pos (v regs val))
          (inc pos))])

(defn step [op regs pos key val]
  (case op
    :sub (-sub regs pos key val)
    :set (-set regs pos key val)
    :mul (-mul regs pos key val)
    :jnz (-jnz regs pos key val)))

(defn solve1 [instructions]
  (loop [regs (sorted-map) pos 0 counter 0]
    (let [[op key val] (get instructions pos)
          counter (+ counter (if (= op :mul) 1 0))]
      (if (nil? op)
        counter
        (let [[regs ^long pos] (step op regs pos key val)]
          (recur regs pos counter))))))

(defn not-prime? [n]
  (if (or (<= n 1) (even? n))
    false
    (some zero?
      (map #(mod n %) (range 2 n)))))

(defn count-non-primes [start end step]
  (count
    (filter not-prime?
      (range start (inc end) step)))) ; inclusive range

(defn solve2
  "This is the assembler program reinterpreted in clojure.
   Counts non-primes between 106700 and 123700, step 17."
  []
  (count-non-primes 106700 123700 17))

(defn main []
  (let [instructions (parse-input (slurp input-file))]
    (println "Mul is invoked" (solve1 instructions) "times")
    (println "H register contains" (solve2))))

; 96 is not the answer :/
