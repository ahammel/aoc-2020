(ns ahammel.aoc-2020.day-11
  (:require [ahammel.aoc-2020.common :refer
             [->2-matrix adjacent sliding-window with-input]]
            [clojure.test :refer [deftest is]]))

(defn tick
  [seat-map]
  (let [adjacent-seats-occupied (fn [seat]
                                  (->> (adjacent seat)
                                       (filter #(= \# (seat-map %)))
                                       count))]
    (into
      (sorted-map)
      (map (fn [seat] [seat
                       (case (seat-map seat)
                         \. \.
                         \L (if (zero? (adjacent-seats-occupied seat)) \# \L)
                         \# (if (<= 4 (adjacent-seats-occupied seat)) \L \#))]))
      (keys seat-map))))


(def fixture
  ["L.LL.LL.LL" ;
   "LLLLLLL.LL" ;
   "L.L.L..L.." ;
   "LLLL.LL.LL" ;
   "L.LL.LL.LL" ;
   "L.LLLLL.LL" ;
   "..L.L....." ;
   "LLLLLLLLLL" ;
   "L.LLLLLL.L" ;
   "L.LLLLL.LL" ;
   ""])

(deftest day-11-test
  (is (= 37
         (let [seat-map (->> (->2-matrix fixture)
                             (iterate tick)
                             (sliding-window 2)
                             (keep (fn [[a b]] (when (= a b) a)))
                             first)]
           (get (frequencies (vals seat-map)) \#))))
  (is (= 2247
         (with-input "day-11.txt"
           (fn [lines]
             (let [seat-map (->> lines
                                 (filter seq)
                                 ->2-matrix
                                 (iterate tick)
                                 (sliding-window 2)
                                 (keep (fn [[a b]] (when (= a b) a)))
                                 first)]
               (-> (vals seat-map)
                   frequencies
                   (get \#))))))))

