(ns ahammel.aoc-2020.day-18
  (:require [ahammel.aoc-2020.common :refer [->int with-input]]
            [clojure.test :refer [deftest is testing]]
            [instaparse.core :as insta]))

(def parse-left-to-right
  (insta/parser
    "Expr = Number | Sum | Product | Parenthetical
     Parenthetical = <'('> (Number | Sum | Product)  <')'>
     Sum = Expr <' + '>  (Number | Parenthetical)
     Product = Expr <' * '>  (Number | Parenthetical)
     Number = #'[0-9]+'"))

(def parse-addition-precedence
  (insta/parser
    "Expr = Number | Sum | Product | Parenthetical
     Parenthetical = <'('> (Number | Sum | Product)  <')'>
     Sum = (Number | Sum | Parenthetical) <' + '>  (Number | Sum | Parenthetical)
     Product = Expr <' * '>  (Number | Sum | Parenthetical)
     Number = #'[0-9]+'"))

(defn evaluate
  [expression]
  (insta/transform
    {:Expr identity, :Parenthetical identity, :Sum +, :Product *, :Number ->int}
    expression))

(deftest day-18-test
  (testing "left-to-right"
    (is (= 71
           (-> "1 + 2 * 3 + 4 * 5 + 6"
               parse-left-to-right
               evaluate)))
    (is (= 51
           (-> "1 + (2 * 3) + (4 * (5 + 6))"
               parse-left-to-right
               evaluate)))
    (is (= 26
           (-> "2 * 3 + (4 * 5)"
               parse-left-to-right
               evaluate)))
    (is (= 437
           (-> "5 + (8 * 3 + 9 + 3 * 4 * 3)"
               parse-left-to-right
               evaluate)))
    (is (= 12240
           (-> "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
               parse-left-to-right
               evaluate)))
    (is (= 13632
           (-> "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
               parse-left-to-right
               evaluate)))
    (is (= 29839238838303
           (with-input "day-18.txt"
             (fn [lines]
               (transduce
                 (comp (filter seq) (map parse-left-to-right) (map evaluate))
                 +
                 lines))))))
  (testing "addition precedence"
    (is (= 231
           (-> "1 + 2 * 3 + 4 * 5 + 6"
               parse-addition-precedence
               evaluate)))
    (is (= 51
           (-> "1 + (2 * 3) + (4 * (5 + 6))"
               parse-addition-precedence
               evaluate)))
    (is (= 46
           (-> "2 * 3 + (4 * 5)"
               parse-addition-precedence
               evaluate)))
    (is (= 1445
           (-> "5 + (8 * 3 + 9 + 3 * 4 * 3)"
               parse-addition-precedence
               evaluate)))
    (is (= 669060
           (-> "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
               parse-addition-precedence
               evaluate)))
    (is (= 23340
           (-> "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
               parse-addition-precedence
               evaluate)))
    (is (= 201376568795521
           (with-input "day-18.txt"
             (fn [lines]
               (transduce (comp (filter seq)
                                (map parse-addition-precedence)
                                (map evaluate))
                          +
                          lines)))))))

