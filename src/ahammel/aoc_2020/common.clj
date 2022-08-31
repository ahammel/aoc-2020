(ns ahammel.aoc-2020.common
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

;; Parsing

(defmulti ->int "Generic integer coercer" type)

(defmethod ->int nil [_] nil)
(defmethod ->int java.lang.String [s] (Integer/parseInt s))

(deftest ->int-test
  (is (nil? (->int nil)))
  (is (zero? (->int "0")))
  (is (= 10 (->int "10"))))

(defn string->int [s] (->int s))

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
