(ns ahammel.aoc-2020.day-6
  (:require [ahammel.aoc-2020.common :refer [with-input]]
            [clojure.set :refer [union intersection]]
            [clojure.test :refer [deftest is]]))

(defn union-counts
  [lines]
  (transduce (comp (partition-by empty?)
                   (filter (fn [line] (not= [""] line)))
                   (map (fn [group] (apply union (map set group))))
                   (map count))
             +
             lines))

(defn intersection-counts
  [lines]
  (transduce (comp (partition-by empty?)
                   (filter (fn [line] (not= [""] line)))
                   (map (fn [group] (apply intersection (map set group))))
                   (map count))
             +
             lines))


(def fixture
  ["abc" ;
   ""    ;
   "a"   ;
   "b"   ;
   "c"   ;
   ""    ;
   "ab"  ;
   "ac"  ;
   ""    ;
   "a"   ;
   "a"   ;
   "a"   ;
   "a"   ;
   ""    ;
   "b"   ;
  ])

(deftest day-6-test
  (is (= 11 (union-counts fixture)))
  (is (= 6549 (with-input "day-6.txt" union-counts)))
  (is (= 6 (intersection-counts fixture)))
  (is (= 3466 (with-input "day-6.txt" intersection-counts))))
