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

(defmulti adjacent
  "Given integral points in an n-dimensional space, returns all the adjacent
  points"
  count)

(defmethod adjacent 2
  [[x y]]
  #{[(inc x) (inc y)] ;
    [(inc x) y]       ;
    [(inc x) (dec y)] ;
    [x (inc y)]       ;
    [x (dec y)]       ;
    [(dec x) (inc y)] ;
    [(dec x) y]       ;
    [(dec x) (dec y)]})

(defmethod adjacent 3
  [[x y z :as point]]
  (into #{}
        (for [x' [(dec x) x (inc x)]
              y' [(dec y) y (inc y)]
              z' [(dec z) z (inc z)]
              :let [point' [x' y' z']]
              :when (not= point point')]
          point')))

(defmethod adjacent 4
  [[x y z w :as point]]
  (into #{}
        (for [x' [(dec x) x (inc x)]
              y' [(dec y) y (inc y)]
              z' [(dec z) z (inc z)]
              w' [(dec w) w (inc w)]
              :let [point' [x' y' z' w']]
              :when (not= point point')]
          point')))

(deftest adjacent-test
  (is (= #{[-1 -1 -1] [0 -1 -1] [1 -1 -1] ;
           [-1 0 -1] [0 0 -1] [1 0 -1] ;
           [-1 1 -1] [0 1 -1] [1 1 -1] ;
           [-1 -1 0] [0 -1 0] [1 -1 0] ;
           [-1 0 0] [1 0 0] ;
           [-1 1 0] [0 1 0] [1 1 0] ;
           [-1 -1 1] [0 -1 1] [1 -1 1] ;
           [-1 0 1] [0 0 1] [1 0 1] ;
           [-1 1 1] [0 1 1] [1 1 1] ;
          }
         (adjacent [0 0 0]))))

(defn point+
  "Matrix addition on two or more two-dimensional points"
  ([] [0 0])
  ([point] point)
  ([[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
  ([point1 point2 & points] (reduce point+ (conj points point1 point2))))

(defn manhattan-distance
  ([point] (manhattan-distance point [0 0]))
  ([[x1 y1] [x2 y2]] [(abs (- x1 x2)) (abs (- y1 y2))]))

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

;; Number theory

(defn gcd "Greatest common divisor" [a b] (if (zero? b) a (recur b (mod a b))))

(defn lcm "Least common multiple" [a b] (* (abs a) (/ b (gcd a b))))
