(ns aoc2017.core
  (:require [aoc2017.day01 :as d1]))

(defn get-ns-symbol [day]
  (symbol
    (str "aoc2017.day"
      (format "%02d"
        (Integer/parseInt day)))))

(defn -main [day & args]
  (if (re-matches #"^\d{1,2}$" day)
    (let [ns-sym (get-ns-symbol day)]
      (try
        (let [main (ns-resolve ns-sym 'main)]
          (println (str "Advent of code 2017, day " day))
          (apply main args))
        (catch Exception e
          (println (str "Cannot find namespace: " ns-sym)))))
    (println "Invalid day given.")))
