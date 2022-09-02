(ns ahammel.aoc-2020.common
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(defn with-input
  [input-file f]
  (with-open [rdr (-> input-file
                      io/resource
                      io/reader)]
    (f (line-seq rdr))))

(comment
  (with-input "day-1.txt"
    (fn [lines]
      (->> lines
           (take 10)
           doall))))

;; Parsing
(defmulti ->int "Generic integer coercer" type)

(defmethod ->int nil [_] nil)

(defmethod ->int java.lang.String
  [s]
  (try (Long/parseLong s) (catch NumberFormatException _ nil)))

(deftest ->int-test
  (is (nil? (->int nil)))
  (is (nil? (->int "foobar")))
  (is (zero? (->int "0")))
  (is (= 10 (->int "10"))))

(defn string->int [s] (->int s))

(defn parse-ints
  "Returns all non-empty lines as integers"
  [lines]
  (eduction (filter seq)
            (map string->int)
            lines))

;; Matrices
(defn ->2-matrix
  [rows]
  (into (sorted-map)
        (comp (filter seq)
              (map-indexed (fn [j row]
                             (->> row
                                  (map-indexed (fn [i x] [[i j] x])))))
              (mapcat identity))
        rows))

(defn extent
  [matrix]
  (->> matrix
       last
       first))

(defn adjacent
  "Given an [int int] point on a two dimensional plane , returns all the
  adjacent adjacent points"
  [[x y]]
  #{[(inc x) (inc y)] ;
    [(inc x) y]       ;
    [(inc x) (dec y)] ;
    [x (inc y)]       ;
    [x (dec y)]       ;
    [(dec x) (inc y)] ;
    [(dec x) y]       ;
    [(dec x) (dec y)]})

(deftest matrix-test
  (is (= {[0 0] \a, [1 0] \b, [0 1] \c, [1 1] \d} (->2-matrix ["ab" "cd"])))
  (is (= [2 2]
         (-> ["abc" "cde" "efg"]
             ->2-matrix
             extent))))

;; Sequence Manipulation
(defn only
  "If the coll is a singleton, return the only element. Else throw an exception."
  [coll]
  (let [head (first coll)
        tail (rest coll)]
    (if (or (nil? head) (seq tail))
      (throw (IllegalArgumentException. (str "Cannot extract only value from "
                                             coll)))
      head)))

;; Sliding windows

(defn sliding-window
  "Returns a lazy sliding window of size n of the items in the coll"
  [n coll]
  (let [colls (map #(drop % coll) (range 0 n))] (apply map vector colls)))

(deftest sliding-window-test
  (is (= [[0 1 2] [1 2 3] [2 3 4]] (take 3 (sliding-window 3 (range))))))
