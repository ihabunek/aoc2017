(ns aoc2017.day05-test
  (:require [clojure.test :refer :all]
            [aoc2017.day05 :refer :all]))

(deftest test-solve
  (is (= (solve [0 3 0 1 -3]) 5)))

