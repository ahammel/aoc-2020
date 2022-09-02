(ns ahammel.aoc-2020.day-9
  (:require [ahammel.aoc-2020.common :refer
             [only parse-ints sliding-window with-input]]
            [clojure.test :refer [deftest is]]))

(defn valid?
  [n preamble]
  (some (fn [x]
          (let [p (-> preamble
                      set
                      (disj x))]
            (contains? p (- n x))))
        preamble))

(defn contiguous-range-sum
  [n preamble]
  (loop [r 2]
    (if (< (count preamble) r)
      nil
      (if-let [solution (->> preamble
                             (sliding-window r)
                             (filter #(= n (reduce + %)))
                             first)]
        solution
        (recur (inc r))))))

(defn check-encoding
  "r is the preamble length"
  [r message]
  (map-indexed
    (fn [ix n]
      (assert n (str "nil found at index: " ix))
      (if (< ix r)
        {::n n, ::valid? true}
        (let [preamble (into [] (comp (drop (- ix r)) (take r)) message)]
          (assert (= r (count preamble)))
          {::n n, ::valid? (valid? n preamble), ::preamble preamble})))
    message))

(def fixture
  [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(deftest day-9-test
  (is (valid? 26 (range 1 26)))
  (is (valid? 49 (range 1 26)))
  (is (not (valid? 100 (range 1 26))))
  (is (not (valid? 50 (range 1 26))))
  (is (= 127
         (->> (check-encoding 5 fixture)
              (remove ::valid?)
              (map ::n)
              only)))
  (is (= 1398413738
         (with-input "day-9.txt"
           (fn [lines]
             (->> (parse-ints lines)
                  (check-encoding 25)
                  (eduction (remove ::valid?)
                            (map ::n)
                            (take 1))
                  only)))))
  (is (= 62
         (let [invalid (->> (check-encoding 5 fixture)
                            (remove ::valid?)
                            first)
               rn (contiguous-range-sum (::n invalid) fixture)]
           (+ (apply min rn) (apply max rn)))))
  (is (= 169521051
         (with-input "day-9.txt"
           (fn [lines]
             (let [message (sequence (parse-ints lines))
                   invalid (->> (check-encoding 25 message)
                                (remove ::valid?)
                                first)
                   rn (contiguous-range-sum (::n invalid) message)]
               (+ (apply min rn) (apply max rn))))))))

