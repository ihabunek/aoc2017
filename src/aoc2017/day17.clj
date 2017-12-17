(ns aoc2017.day17)

(defn rotate [coll n]
  (if (empty? coll)
    coll
    (let [n (mod n (count coll))]
      (concat
        (drop n coll)
        (take n coll)))))

(defn solve1 [n delta]
  (loop [buffer () value 0]
    (if (> value n)
      (second buffer)
      (recur
        (cons value (rotate buffer (inc delta)))
        (inc value)))))

(defn solve2 [n delta]
  (loop [value 1 pos 1 second 1]
    (if (> value n)
      second
      (recur
        (inc value)
        (inc (rem (+ pos delta) (inc value)))
        (if (= pos 1) value second)))))

(defn main []
  (println "Part 1:" (time (solve1 2017 343)))
  (println "Part 2:" (time (solve2 50000000 343))))
