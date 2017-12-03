(ns aoc2017.core)

(defn -main [day & args]
  (let [ns-sym (symbol (str "aoc2017." day))]
    (try
      (require ns-sym)
      (println "Advent of code 2017," day)
      (apply (ns-resolve ns-sym 'main) args))))
