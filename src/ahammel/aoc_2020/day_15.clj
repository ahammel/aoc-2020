(ns ahammel.aoc-2020.day-15
  (:require [clojure.test :refer [deftest is]]))

(defn recite
  [init]
  (map first
    (iterate (fn [[x xs calls round]]
               (let [y (if (seq xs)
                         (first xs)
                         (if-let [last-called (get calls x)]
                           (- round last-called)
                           0))]
                 [y (rest xs) (assoc calls x round) (inc round)]))
             [(first init) (rest init) {} 1])))

(def fixture [0 3 6])

(def input [2 15 0 9 1 20])

(deftest day-15-test
  (is (= [0 3 6 0 3 3 1 0 4 0] (take 10 (recite fixture))))
  (is (= 436 (nth (recite fixture) (dec 2020))))
  (is (= 1280 (nth (recite input) (dec 2020))))
  (is (= 651639 (nth (recite input) (dec 30000000)))))
