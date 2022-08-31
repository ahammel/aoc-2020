(ns ahammel.aoc-2020.day-2
  (:require [ahammel.aoc-2020.common :refer [->int with-input]]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(defn parse
  [lines]
  (let [parse' (fn [line]
                 (let [[policy password] (string/split line #": ")
                       [min-max character] (string/split policy #" ")
                       [a b] (string/split min-max #"-")]
                   {::character (first character),
                    ::a (->int a),
                    ::b (->int b),
                    ::password password}))]
    (sequence (comp (filter seq) (map parse')) lines)))

(defn valid-count?
  [{::keys [character a b password]}]
  (let [n (->> password
               (keep #{character})
               count)]
    (and (<= a n) (<= n b))))

(defn xor [a b] (and (or a b) (not (and a b))))

(defn valid-position?
  [{::keys [character a b password]}]
  (let [char-a (get password (dec a))
        char-b (get password (dec b))]
    (xor (= character char-a) (= character char-b))))

(def fixture ["1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc" ""])

(deftest day-2-test
  (is (= [{::character \a, ::a 1, ::b 3, ::password "abcde"}
          {::character \b, ::a 1, ::b 3, ::password "cdefg"}
          {::character \c, ::a 2, ::b 9, ::password "ccccccccc"}]
         (into [] (parse fixture))))
  (is (= 2
         (->> (parse fixture)
              (filter valid-count?)
              count)))
  (is (= 383
         (with-input "day-2.txt"
           (fn [lines]
             (->> (parse lines)
                  (filter valid-count?)
                  count)))))
  (is (= 1
         (->> (parse fixture)
              (filter valid-position?)
              count)))
  (is (= 272
         (with-input "day-2.txt"
           (fn [lines]
             (->> (parse lines)
                  (filter valid-position?)
                  count))))))
