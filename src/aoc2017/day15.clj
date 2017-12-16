(ns aoc2017.day15)

; Inputs
(def start-a 722)
(def start-b 354)
(def inc-a 16807)
(def inc-b 48271)
(def gen-mod 2147483647)

(defn generator [start delta divisor]
  (iterate
    (fn [x] (mod (*' x delta) divisor))
    start))

  ; (let [next (mod (* start diff) m)]
  ;   (lazy-seq
  ;     (cons next
  ;       (generator next diff m)))))

(defn generator2 [start diff m1 m2]
  (filter #(zero? (mod % m2))
    (generator start diff m1)))

(defn same-lower-bits? [a b]
  (= (bit-and a 0xffff) (bit-and b 0xffff)))
  ; alternatives:
  ; (= (unchecked-short a) (unchecked-short b)))
  ; (= (mod a 65536) (mod b 65536)))

(defn solve [gen-a gen-b]
  (loop [gen-a gen-a gen-b gen-b counter 0]
    (let [a (first gen-a) b (first gen-b)]
      (if (nil? a)
        counter
        (recur
          (rest gen-a)
          (rest gen-b)
          (if (same-lower-bits? a b) (inc counter) counter))))))

(defn solve1 []
  (solve
    (take 40000000 (generator start-a inc-a gen-mod))
    (take 40000000 (generator start-b inc-b gen-mod))))

(defn solve2 []
  (solve
    (take 5000000 (generator2 start-a inc-a gen-mod 4))
    (take 5000000 (generator2 start-b inc-b gen-mod 8))))

(defn main []
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true)
  (println "Count 1:" (time (solve1)))
  (println "Count 2:" (time (solve2))))
