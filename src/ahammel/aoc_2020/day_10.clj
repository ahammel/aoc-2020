(ns ahammel.aoc-2020.day-10
  (:require [ahammel.aoc-2020.common :refer
             [parse-ints sliding-window with-input]]
            [clojure.test :refer [deftest is]]))

(defn joltage-differences
  [adaptors]
  (let [adaptors' (sort adaptors)
        joltages (concat [0] adaptors' [(+ 3 (last adaptors'))])]
    (->> joltages
         (sliding-window 2)
         (map (fn [[a b]] (- b a)))
         frequencies)))

(defn connections [n adaptors] (filter #(<= 1 (- % n) 3) adaptors))

(defn ->tree
  [adaptors]
  (let [adaptors' (concat [0] adaptors [(+ 3 (apply max adaptors))])]
    (transduce (mapcat #(for [out (connections % adaptors)] [% out]))
               (fn
                 ([] (sorted-map))
                 ([acc] acc)
                 ([acc [in out]] (update acc in conj out)))
               adaptors')))

(defn paths-to-map
  [adaptors]
  (let [tree (->tree adaptors)]
    (reduce (fn [paths-to adaptor]
              (let [updates (into {}
                                  (map #(vector % (paths-to adaptor)))
                                  (tree adaptor))]
                (merge-with + paths-to updates)))
      (sorted-map 0 1)
      (keys tree))))

(def fixture-1 [16 10 15 5 1 11 7 19 6 12 4])

(def fixture-2
  [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2
   34 10 3])

(deftest day-10-test
  (is (= {1 7, 3 5} (joltage-differences fixture-1)))
  (is (= {1 22, 3 10} (joltage-differences fixture-2)))
  (is (= 2312
         (with-input "day-10.txt"
           (fn [lines]
             (let [{ones 1, threes 3} (joltage-differences (parse-ints lines))]
               (* ones threes))))))
  (is (= [19 8]
         (-> fixture-1
             paths-to-map
             last)))
  (is (= [49 19208]
         (-> fixture-2
             paths-to-map
             last)))
  (is (= [167 12089663946752]
         (with-input "day-10.txt"
           (fn [lines]
             (-> lines
                 parse-ints
                 paths-to-map
                 last))))))
