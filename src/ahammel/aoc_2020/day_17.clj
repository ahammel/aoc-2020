(ns ahammel.aoc-2020.day-17
  (:require [ahammel.aoc-2020.common :refer [->2-matrix adjacent]]
            [clojure.set :as set]
            [clojure.test :refer [deftest is]]))

(defn envelope
  "Returns the set of inactive points which are adjacent to an active point
  given a Conway space (represented as a set of active points)"
  [cspace]
  (into #{} (comp (mapcat adjacent) (remove cspace)) cspace))

(defn active-neighbors
  "Returns a lazy seq of the neighbors of the point which are active in the
  given Conway space"
  [cspace point]
  (filter cspace (adjacent point)))

(defn tick
  "Advances the state of the Conway space by one time unit"
  [cspace]
  (let [neighbors (fn [point] (count (active-neighbors cspace point)))
        activations (into #{} (filter #(= 3 (neighbors %))) (envelope cspace))
        deactivations (into #{} (remove #(<= 2 (neighbors %) 3)) cspace)]
    (-> cspace
        (set/union activations)
        (set/difference deactivations))))

(def fixture
  [".#." ;
   "..#" ;
   "###" ;
  ])

(def input
  ["#......." ;
   ".#..#..#" ;
   "....#.#." ;
   ".##..#.#" ;
   "#######." ;
   "#...####" ;
   "###.##.." ;
   ".##.#.#." ;
  ])

(defn parse-3d
  [lines]
  (into #{}
        (keep (fn [[point value]]
                (let [[x y] point] (when (= \# value) [x y 0]))))
        (->2-matrix lines)))

(defn parse-4d
  [lines]
  (into #{}
        (keep (fn [[point value]]
                (let [[x y] point] (when (= \# value) [x y 0 0]))))
        (->2-matrix lines)))

(deftest day-17-test
  (is (= 112
         (->> (parse-3d fixture)
              (iterate tick)
              (take (inc 6))
              last
              count)))
  (is (= 218
         (->> (parse-3d input)
              (iterate tick)
              (take (inc 6))
              last
              count)))
  (is (= 848
         (->> (parse-4d fixture)
              (iterate tick)
              (take (inc 6))
              last
              count)))
  (is (= 1908
         (->> (parse-4d input)
              (iterate tick)
              (take (inc 6))
              last
              count))))

