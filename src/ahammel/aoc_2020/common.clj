(ns ahammel.aoc-2020.common
  (:require [clojure.java.io :as io]))

;; Parsing

(defn string->int [s] (when s (Integer/parseInt s)))

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

(defn parse-ints
  "Returns all non-empty lines as integers"
  [lines]
  (eduction (filter seq) (map string->int) lines))
