(ns aoc2017.day06)

(def input [4 1 15 12 0 9 9 5 5 8 7 3 14 5 12 3])

(defn max-index
  "Find the index of the bank which has the most blocks."
  [banks]
  (let [max-value (apply max banks)]
    (first
      (keep-indexed
        #(if (= %2 max-value) %1)
        banks))))

(defn redistribute
  "Redistribute blocks from given index circulary"
  [banks idx]
  (loop [blocks (get banks idx)
         banks (assoc banks idx 0)
         idx (inc idx)]
    (if (= blocks 0) banks
      (let [idx (mod idx (count banks))]
        (recur (dec blocks)
               (assoc banks idx (inc (get banks idx)))
               (inc idx))))))

(defn reallocate
  "Reallocates blocks, returns the steps taken and cycle size."
  [banks]
  (loop [banks banks
         history [banks]]
    (let [idx (max-index banks)
          next-banks (redistribute banks idx)
          existing-idx (.indexOf history next-banks)]
      (if (>= existing-idx 0)
        (list
          (count history) ; number of steps
          (- (count history) existing-idx)) ; cycle size
        (recur
          next-banks
          (conj history next-banks))))))

(defn main []
  (let [[steps cycle-size] (time (reallocate input))]
    (println "It took" steps "steps, cycles size is" cycle-size)))
