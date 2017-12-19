(ns aoc2017.day18
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split split-lines]]
            [clojure.pprint :refer [pprint]]
            [aoc2017.util :refer [parse-int]]))

(def input-file (-> "day18.in" io/resource io/file))

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

(defn -add [regs pos key val]
  (generic regs pos key val +))

(defn -mul [regs pos key val]
  (generic regs pos key val *))

(defn -mod [regs pos key val]
  (generic regs pos key val mod))

(defn -nop [regs pos]
  [regs (inc pos)])

(defn -jgz [regs pos key val]
  [regs (if (pos? (v regs key))
          (+ pos (v regs val))
          (inc pos))])

(defn step [op regs pos key val]
  (case op
    :add (-add regs pos key val)
    :set (-set regs pos key val)
    :mul (-mul regs pos key val)
    :mod (-mod regs pos key val)
    :jgz (-jgz regs pos key val)
    :rcv (-nop regs pos)
    :snd (-nop regs pos)))

(defn solve1 [instructions]
  (loop [regs {} pos 0 sound nil]
    (let [[op key val] (get instructions pos)]
      (if (and (= op :rcv) (not (zero? (regs key))))
        sound
        (let [sound (if (= op :snd) (get regs key) sound)
              [regs pos] (step op regs pos key val)]
          (recur regs pos sound))))))

(defn buffer []
  clojure.lang.PersistentQueue/EMPTY)

(defn blocked? [op buffer]
  (and (= op :rcv) (empty? buffer)))

(defn step2 [op regs pos key val]
  (case op
    :add (-add regs pos key val)
    :set (-set regs pos key val)
    :mul (-mul regs pos key val)
    :mod (-mod regs pos key val)
    :jgz (-jgz regs pos key val)
    :rcv (-set regs pos key val)
    :snd (-nop regs pos)))

(defn exhaust [instructions regs pos buffer]
  (loop [regs regs pos pos buffer buffer counter 0 sent []]
    (let [[op key val] (get instructions pos)]
      (if (blocked? op buffer)
        [regs pos buffer counter sent]
        (let [val    (if (= op :rcv) (peek buffer) val)
              buffer (if (= op :rcv) (pop buffer) buffer)
              sent   (if (= op :snd) (conj sent (v regs key)) sent)
              [next-regs next-pos] (step2 op regs pos key val)]
          (recur next-regs next-pos buffer (inc counter) sent))))))

(defn solve2 [instructions]
  (loop [regs1 {:p 0} pos1 0 buffer1 (buffer)
         regs2 {:p 1} pos2 0 buffer2 (buffer)
         sent-counter-1 0 sent-counter-2 0]
    (let [[regs1 pos1 buffer1 counter1 sent1] (exhaust instructions regs1 pos1 buffer1)
          buffer2 (apply conj buffer2 sent1)
          sent-counter-1 (+ sent-counter-1 (count sent1))]
      (let [[regs2 pos2 buffer2 counter2 sent2] (exhaust instructions regs2 pos2 buffer2)
            buffer1 (apply conj buffer1 sent2)
            sent-counter-2 (+ sent-counter-2 (count sent2))]
        (if (and (zero? counter1) (zero? counter2))
          [sent-counter-1 sent-counter-2]
          (recur regs1 pos1 buffer1 regs2 pos2 buffer2 sent-counter-1 sent-counter-2))))))

(defn main []
  (let [instructions (parse-input (slurp input-file))]
    (println "Last sound emitted was" (solve1 instructions))
    (println "Number of sent items by program 1:" (second (solve2 instructions)))))
