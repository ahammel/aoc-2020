(ns ahammel.aoc-2020.day-1
  (:require [ahammel.aoc-2020.common :refer [parse-ints with-input]]
            [clojure.test :refer [deftest is]]))

(defn ^:private increment-coll
  [tuple n]
  (let [col (rest tuple)
        r (count col)
        maxes (into [] (take r) (range (- n 2) 0 -1))
        inc-col (fn inc-col [col]
                  (let [[ix i] (first col)
                        max-i (get maxes ix)]
                    (lazy-seq
                      (cond (< i max-i) (cons (inc i) (map second (rest col)))
                            (< ix (dec r)) (let [rest' (inc-col (rest col))]
                                             (cons (inc (first rest')) rest'))
                            :else [0]))))
        col' (inc-col (map-indexed vector col))]
    (when (not= maxes col) (cons (inc (first col')) col'))))


(defn tuple-sum
  [xs tuple-size target]
  (let [coll (into [] (sort xs))
        iter (fn [tuple]
               (cond (nil? tuple) nil
                     (some #(not (<= 0 % (inc (count coll)))) tuple) nil
                     :else
                       (let [row (first tuple)
                             col (rest tuple)
                             tuple' (mapv (fn [i] (when i (get coll i))) tuple)
                             sum (reduce + tuple')]
                         (cond (= sum target) tuple'
                               (< sum target) (recur (cons (inc row) col))
                               (> sum target)
                                 (recur (increment-coll tuple (count coll)))))))
        init (range (dec tuple-size) -1 -1)]
    (iter init)))

(defn day-1-a [xs] (when-let [tuple (tuple-sum xs 2 2020)] (reduce * tuple)))

(defn day-1-b [xs] (when-let [tuple (tuple-sum xs 3 2020)] (reduce * tuple)))

(def fixture [1721 979 366 299 675 1456])

(deftest day-1-test
  (is (= 514579 (day-1-a fixture)))
  (is (= 921504
         (with-input "day-1.txt" (fn [lines] (day-1-a (parse-ints lines))))))
  (is (= 241861950 (day-1-b fixture)))
  (is (= 195700142
         (with-input "day-1.txt" (fn [lines] (day-1-b (parse-ints lines)))))))
