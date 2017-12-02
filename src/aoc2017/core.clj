(ns aoc2017.core)

(defn get-ns-symbol [day]
  (symbol
    (str "aoc2017.day"
      (format "%02d"
        (Integer/parseInt day)))))

(defn -main [day & args]
  (if (re-matches #"^\d{1,2}$" day)
    (let [ns-sym (get-ns-symbol day)]
      (try
        (require ns-sym)
        (println (str "Advent of code 2017, day " day))
        (apply (ns-resolve ns-sym 'main) args)
        (catch Exception e
          (println (.getMessage e)))))
    (println "Invalid day given.")))
