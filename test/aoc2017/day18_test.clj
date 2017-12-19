(ns aoc2017.day18-test
  (:require [clojure.test :refer :all]
            [aoc2017.day18 :refer :all]))

(deftest test-set
  (is (= (-set {} 0 :a 1) [{:a 1} 1]))
  (is (= (-set {:a 2} 1 :a 1) [{:a 1} 2]))
  (is (= (-set {:b 2} 0 :a :b) [{:a 2 :b 2} 1]))
  (is (= (-set {:a 1 :b 2} 0 :a 3) [{:a 3 :b 2} 1]))
  (is (= (-set {:a 1 :b 2} 0 :a :b) [{:a 2 :b 2} 1]))
  (is (= (-set {:a 1} 0 :a :b) [{:a 0} 1]))
  (is (= (-set {} 0 :a :b) [{:a 0} 1])))

(deftest test-add
  (is (= (-add {} 0 :a 1) [{:a 1} 1]))
  (is (= (-add {:a 2} 1 :a 1) [{:a 3} 2]))
  (is (= (-add {:b 2} 0 :a :b) [{:a 2 :b 2} 1]))
  (is (= (-add {:a 1 :b 2} 0 :a :b) [{:a 3 :b 2} 1]))
  (is (= (-add {:a 1} 0 :a :b) [{:a 1} 1])))

(deftest test-mul
  (is (= (-mul {} 0 :a 10) [{:a 0} 1]))
  (is (= (-mul {:a 2} 1 :a 3) [{:a 6} 2]))
  (is (= (-mul {:b 2} 0 :a :b) [{:a 0 :b 2} 1]))
  (is (= (-mul {:a 3 :b 2} 0 :a :b) [{:a 6 :b 2} 1]))
  (is (= (-mul {:a 3} 0 :a :b) [{:a 0} 1])))

(deftest test-mod
  (is (= (-mod {} 0 :a 10) [{:a 0} 1]))
  (is (= (-mod {:a 7} 1 :a 3) [{:a 1} 2]))
  (is (= (-mod {:b 2} 0 :a :b) [{:a 0 :b 2} 1]))
  (is (= (-mod {:a 3 :b 2} 0 :a :b) [{:a 1 :b 2} 1])))
  ; (is (= (-mod {:a 3} 0 :a :b) [{:a 3} 1]))) ; divide by zero

(deftest test-nop
  (is (= (-nop {} 0)      [{} 1]))
  (is (= (-nop {:a 0} 1)  [{:a 0} 2]))
  (is (= (-nop {:a 0 :b 1} 2)  [{:a 0 :b 1} 3])))

(deftest test-jgz
  (is (= (-jgz {} 0 :a 5)      [{} 1]))
  (is (= (-jgz {:a -1} 0 :a 5) [{:a -1} 1]))
  (is (= (-jgz {:a 0} 0 :a 5)  [{:a 0} 1]))
  (is (= (-jgz {:a 1} 0 :a 5)  [{:a 1} 5]))
  (is (= (-jgz {:a 1} 1 :a 5)  [{:a 1} 6]))
  (is (= (-jgz {} 0 0 5)      [{} 1]))
  (is (= (-jgz {} 0 -1 5)      [{} 1]))
  (is (= (-jgz {} 0 1 5)      [{} 5]))
  (is (= (-jgz {} 1 -1 -1)      [{} 2]))
  (is (= (-jgz {} 1 0 -1)      [{} 2]))
  (is (= (-jgz {} 1 1 -1)      [{} 0])))
