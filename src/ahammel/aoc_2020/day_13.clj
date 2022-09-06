(ns ahammel.aoc-2020.day-13
  (:require [ahammel.aoc-2020.common :refer [->int lcm only with-input]]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(defn next-bus
  [{::keys [t busses]}]
  (->> busses
       (eduction (remove nil?)
                 (map #(vector (mod (- t) %) %)))
       sort
       (take 1)
       only))

(defn weird-number-theory-function
  [[mininmum base] [offset bus-id]]
  (let [next-min (->> (iterate #(+' base %) mininmum)
                      (eduction (take 1000000)
                                (filter #(= (mod offset bus-id)
                                            (mod (- %) bus-id)))
                                (take 1))
                      only)]
    [next-min (lcm base bus-id)]))

(defn solve-consecutive-busses
  [indexed-busses]
  (first (reduce weird-number-theory-function [0 1] indexed-busses)))

(defn parse
  [lines]
  {::t (-> lines
           first
           ->int),
   ::busses (map #(->int %)
              (-> lines
                  second
                  (string/split #",")))})

(def fixture ["939" "7,13,x,x,59,x,31,19"])

(deftest day-13-test
  (is (= 295
         (->> (parse fixture)
              next-bus
              (reduce *))))
  (is (= 4722
         (with-input "day-13.txt"
           (fn [lines]
             (->> (parse lines)
                  next-bus
                  (reduce *))))))
  (is (= 3417 (solve-consecutive-busses [[0 17] [2 13] [3 19]])))
  (is (= 754018 (solve-consecutive-busses [[0 67] [1 7] [2 59] [3 61]])))
  (is (= 779210 (solve-consecutive-busses [[0 67] [2 7] [3 59] [4 61]])))
  (is (= 1261476 (solve-consecutive-busses [[0 67] [1 7] [3 59] [4 61]])))
  (is (= 1202161486
         (solve-consecutive-busses [[0 1789] [1 37] [2 47] [3 1889]])))
  (is (= 825305207525452
         (with-input "day-13.txt"
           (fn [lines]
             (->> (parse lines)
                  ::busses
                  (map-indexed vector)
                  (remove (fn [[_ b]] (nil? b)))
                  solve-consecutive-busses))))))
