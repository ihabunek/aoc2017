(ns aoc2017.day16-test
  (:require [clojure.test :refer :all]
            [aoc2017.day16 :refer :all]))

(deftest test-operations
  (is (= (spin [:a :b :c :d :e] 1) [:e :a :b :c :d]))
  (is (= (exchange [:e :a :b :c :d] 3 4) [:e :a :b :d :c]))
  (is (= (partner [:e :a :b :d :c] :e :b) [:b :a :e :d :c])))
