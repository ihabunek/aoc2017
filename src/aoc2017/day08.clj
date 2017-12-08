(ns aoc2017.day08
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split split-lines]]
            [aoc2017.util :refer [parse-int]]))

(def input-file (-> "day08.in" io/resource io/file))

(defn parse-op [op]
  (case op
    "inc" +
    "dec" -
    "==" =
    "!=" not=
    (eval (symbol op))))

(defn parse-command [line]
  (let [parts (split line #" ")]
    [(keyword (parts 0))      ; target register
     (parse-op (parts 1))     ; operation
     (parse-int (parts 2))    ; amount
     (keyword (parts 4))      ; check register
     (parse-op (parts 5))     ; check operation
     (parse-int (parts 6))])) ; check amount

(defn parse-input [input]
  (->> input
       split-lines
       (mapv parse-command)))

(defn command-cond [regs [reg op amount]]
  (op (regs reg 0) amount))

(defn apply-command' [regs [reg op amount]]
  (assoc regs reg (op (regs reg 0) amount)))

(defn apply-command [regs command]
  (if (command-cond regs (drop 3 command))
    (apply-command' regs (take 3 command))
    regs))

(defn get-max [regs max-val]
  (max max-val
    (if (empty? regs) 0
      (apply max (vals regs)))))

(defn apply-commands [regs commands]
  (loop [regs regs commands commands max-val 0]
    (if (empty? commands)
      [regs (get-max regs max-val)]
      (recur
        (apply-command regs (first commands))
        (rest commands)
        (get-max regs max-val)))))

(defn main []
  (let [commands (parse-input (slurp input-file))
        [regs max-val] (apply-commands {} commands)]
    (println "The largest value at the end is" (get-max regs 0))
    (println "The largest value during the proces is" max-val)))
