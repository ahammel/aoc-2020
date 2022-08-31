(ns ahammel.aoc-2020.day-5
  (:require [ahammel.aoc-2020.common :refer [only with-input]]
            [clojure.test :refer [deftest is]]))

(defn cycle-
  [cycle-length]
  (cycle (concat (repeat cycle-length "F") (repeat cycle-length "B"))))

(def row-id
  (->> (map (fn [& code] (apply str code))
         (cycle- 64)
         (cycle- 32)
         (cycle- 16)
         (cycle- 8)
         (cycle- 4)
         (cycle- 2)
         (cycle- 1))
       (map-indexed (fn [i code] [code i]))
       (take 128)
       (into {})))

(def column-id
  {"LLL" 0, "LLR" 1, "LRL" 2, "LRR" 3, "RLL" 4, "RLR" 5, "RRL" 6, "RRR" 7})

(def seat-id
  (into {}
        (for [[row-string row] row-id
              [col-string col] column-id]
          [(str row-string col-string) (+ col (* 8 row))])))

(deftest day-5-test
  (is (= 357 (seat-id "FBFBBFFRLR")))
  (is (= 567 (seat-id "BFFFBBFRRR")))
  (is (= 119 (seat-id "FFFBBBFRRR")))
  (is (= 820 (seat-id "BBFFBBFRLL")))
  (is (= 874
         (with-input "day-5.txt"
           (fn [lines]
             (->> lines
                  (eduction (filter seq) (map seat-id))
                  (apply max))))))
  (is (= 594
         (with-input "day-5.txt"
           (fn [lines]
             (let [seen-ids (into #{} (comp (filter seq) (map seat-id)) lines)]
               (->> (vals seat-id)
                    (eduction (remove seen-ids)
                              (filter (fn [id]
                                        (and (seen-ids (inc id))
                                             (seen-ids (dec id))))))
                    only)))))))
