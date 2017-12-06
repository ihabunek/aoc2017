(ns aoc2017.day05-test
  (:require [clojure.test :refer :all]
            [aoc2017.day06 :refer :all]))

(deftest test-max-index
  (is (= (max-index [0 2 8 0]) 2))
  (is (= (max-index [0 8 8 0]) 1))
  (is (= (max-index [8 8 8 8]) 0)))

(deftest test-reallocate
  (is (= (reallocate [0 2 8 0]) 5)))
