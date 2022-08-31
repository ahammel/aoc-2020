(ns ahammel.aoc-2020.day-3
  (:require [ahammel.aoc-2020.common :refer [->2-matrix extent with-input]]
            [clojure.test :refer [deftest is]]))

(defn path
  [slope matrix]
  (let [[right down] slope
        [max-i max-j] (extent matrix)
        modulus (inc max-i)
        increment (fn [[i j]] [(mod (+ i right) modulus) (+ j down)])
        in-bounds? (fn [[_ j]] (<= j max-j))]
    (->> (iterate increment [0 0])
         (sequence (comp (take-while in-bounds?)
                         (map (fn [point] [point (matrix point)])))))))

(defn trees-in-path
  [path-spec matrix]
  (->> matrix
       (path path-spec)
       (filter (fn [[_ cell]] (= \# cell)))
       count))

(def fixture
  ["..##......." ; ..##.........##.........##.........##.........##.........##.......
   "#...#...#.." ; #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
   ".#....#..#." ; .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
   "..#.#...#.#" ; ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
   ".#...##..#." ; .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
   "..#.##....." ; ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....
   ".#.#.#....#" ; .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
   ".#........#" ; .#........#.#........X.#........#.#........#.#........#.#........#
   "#.##...#..." ; #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
   "#...##....#" ; #...##....##...##....##...#X....##...##....##...##....##...##....#
   ".#..#...#.#" ; .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#
   ""])

(deftest day-3-test
  (is (= 7 (trees-in-path [3 1] (->2-matrix fixture))))
  (is (= 282
         (with-input "day-3.txt"
           (fn [lines] (trees-in-path [3 1] (->2-matrix lines))))))
  (is (= 336
         (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
              (map #(trees-in-path % (->2-matrix fixture)))
              (reduce *))))
  (is (= 958815792
         (with-input "day-3.txt"
           (fn [lines]
             (let [matrix (->2-matrix lines)]
               (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
                    (map #(trees-in-path % matrix))
                    (reduce *))))))))
