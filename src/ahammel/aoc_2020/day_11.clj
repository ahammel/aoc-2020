(ns ahammel.aoc-2020.day-11
  (:require [ahammel.aoc-2020.common :refer
             [->2-matrix adjacent sliding-window with-input]]
            [clojure.test :refer [deftest is]]))

(defn tick
  [seat-map neighbors max-occupied-seats]
  (let [adjacent-seats-occupied (fn [seat]
                                  (->> (neighbors seat)
                                       (filter #(= \# (seat-map %)))
                                       count))]
    (into
      (sorted-map)
      (map (fn [seat] [seat
                       (case (seat-map seat)
                         \. \.
                         \L (if (zero? (adjacent-seats-occupied seat)) \# \L)
                         \# (if (<= max-occupied-seats
                                    (adjacent-seats-occupied seat))
                              \L
                              \#))]))
      (keys seat-map))))

(defn line-of-sight-neighbors
  [seat-map [x y]]
  (let [neighbor (fn [[a b]]
                   (loop [[x' y' :as point'] [(+ x a) (+ y b)]]
                     (case (seat-map point')
                       nil nil
                       \L point'
                       \# point'
                       \. (recur [(+ x' a) (+ y' b)]))))]
    (keep neighbor [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])))

(defn solve-adjacent-neighbors
  [seat-map]
  (let [neighbors (memoize (fn [point]
                             (filter #(-> %
                                          seat-map
                                          #{\# \L})
                               (adjacent point))))]
    (->> seat-map
         (iterate #(tick % neighbors 4))
         (sliding-window 2)
         (keep (fn [[a b]] (when (= a b) a)))
         first)))

(defn solve-line-of-sight-neighbors
  [seat-map]
  (let [neighbors (memoize #(line-of-sight-neighbors seat-map %))]
    (->> seat-map
         (iterate #(tick % neighbors 5))
         (sliding-window 2)
         (keep (fn [[a b]] (when (= a b) a)))
         first)))

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

(deftest day-11-test-fixture
  (is (= 37
         (-> (->2-matrix fixture)
             solve-adjacent-neighbors
             vals
             frequencies
             (get \#))))
  (is (= 26
         (-> (->2-matrix fixture)
             solve-line-of-sight-neighbors
             vals
             frequencies
             (get \#)))))


(deftest ^:slow day-11-test-input
  (is (= 2247
         (with-input "day-11.txt"
           (fn [lines]
             (-> (->2-matrix lines)
                 solve-adjacent-neighbors
                 vals
                 frequencies
                 (get \#))))))
  (is (= 2011
         (with-input "day-11.txt"
           (fn [lines]
             (-> (->2-matrix lines)
                 solve-line-of-sight-neighbors
                 vals
                 frequencies
                 (get \#)))))))
