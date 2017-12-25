(ns aoc2017.day25)

; Input indexed by: current state, then current value
; Vector contains value to write, position to move and next state
(def input {:A {0 [1  1 :B]  1 [0 -1 :C]}
            :B {0 [1 -1 :A]  1 [1  1 :D]}
            :C {0 [0 -1 :B]  1 [0 -1 :E]}
            :D {0 [1  1 :A]  1 [0  1 :B]}
            :E {0 [1 -1 :F]  1 [1 -1 :C]}
            :F {0 [1  1 :D]  1 [1  1 :A]}})

(defn tget [tape pos]
  (if (contains? tape pos) 1 0))

(defn tset [tape pos val]
  (if (= val 1)
    (conj tape pos)
    (disj tape pos)))

(defn solve [rules state n]
  (loop [tape #{} state state pos 0 counter 0]
    (if (= counter n)
      (count tape)
      (let [val (tget tape pos)
            [write ^long move state] (get-in rules [state val])]
          (recur
            (tset tape pos write)
            state
            (+ pos move)
            (inc counter))))))

(defn main []
  (println "The checksum is" (time (solve input :A 12667664))))
