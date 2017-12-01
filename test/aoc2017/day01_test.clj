(ns aoc2017.day01-test
  (:require [clojure.test :refer :all]
            [aoc2017.day01 :refer :all]))

(deftest test-sum-1
  (is (= (sum (parse-input "1122") get-next) 3))
  (is (= (sum (parse-input "1111") get-next) 4))
  (is (= (sum (parse-input "1234") get-next) 0))
  (is (= (sum (parse-input "91212129") get-next) 9)))

(deftest test-sum-2
  (is (= (sum (parse-input "1212") get-opposite) 6))
  (is (= (sum (parse-input "1221") get-opposite) 0))
  (is (= (sum (parse-input "123425") get-opposite) 4))
  (is (= (sum (parse-input "123123") get-opposite) 12))
  (is (= (sum (parse-input "12131415") get-opposite) 4)))
