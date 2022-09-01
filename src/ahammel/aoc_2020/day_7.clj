(ns ahammel.aoc-2020.day-7
  (:require [ahammel.aoc-2020.common :refer [->int with-input]]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(defn can-contain?
  [rules container containee]
  (let [contents (rules (:bag/colour container))]
    (or (some #(= (:bag/colour containee) (:bag/colour %)) contents)
        (some #(can-contain? rules % containee) contents))))

(defn bags-inside
  [rules bag]
  (let [contents (rules (:bag/colour bag))]
    (+ (transduce (map :bag/count) + contents)
       (reduce +
         (lazy-seq (map (fn [bag']
                          (* (:bag/count bag') (bags-inside rules bag')))
                     contents))))))

(defn parse
  [lines]
  (let [parse-containee (fn [containee]
                          (when-let [[_ n colour] (re-matches
                                                    #"(\d+) (\w+ \w+) bags?"
                                                    containee)]
                            {:bag/colour colour, :bag/count (->int n)}))
        parse-line
          (fn [line]
            (let [[container containee-string] (string/split line
                                                             #" bags contain ")
                  containees (when (not= containee-string "no other bags.")
                               (-> containee-string
                                   (string/replace #"\.$" "")
                                   (string/split #", ")))]
              [container (map parse-containee containees)]))]
    (into {} (comp (filter seq) (map parse-line)) lines)))

(def fixture-1
  ["light red bags contain 1 bright white bag, 2 muted yellow bags."    ;
   "dark orange bags contain 3 bright white bags, 4 muted yellow bags." ;
   "bright white bags contain 1 shiny gold bag."                        ;
   "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."    ;
   "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."     ;
   "dark olive bags contain 3 faded blue bags, 4 dotted black bags."    ;
   "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."  ;
   "faded blue bags contain no other bags."                             ;
   "dotted black bags contain no other bags."                           ;
   ""])

(def fixture-2
  ["shiny gold bags contain 2 dark red bags."     ;
   "dark red bags contain 2 dark orange bags."    ;
   "dark orange bags contain 2 dark yellow bags." ;
   "dark yellow bags contain 2 dark green bags."  ;
   "dark green bags contain 2 dark blue bags."    ;
   "dark blue bags contain 2 dark violet bags."   ;
   "dark violet bags contain no other bags."      ;
  ])


(deftest day-7-test
  (is (= 4
         (let [rules (parse fixture-1)]
           (->> (keys rules)
                (filter #(can-contain? rules
                                       {:bag/colour %}
                                       {:bag/colour "shiny gold"}))
                count))))
  (is (= 272
         (with-input "day-7.txt"
           (fn [lines]
             (let [rules (parse lines)]
               (->> (keys rules)
                    (filter #(can-contain? rules
                                           {:bag/colour %}
                                           {:bag/colour "shiny gold"}))
                    count))))))
  (is (= 32
         (->> (bags-inside (parse fixture-1) {:bag/colour "shiny gold"}))))
  (is (= 126
         (->> (bags-inside (parse fixture-2) {:bag/colour "shiny gold"}))))
  (is
    (= 172246
       (with-input "day-7.txt"
         (fn [lines] (bags-inside (parse lines) {:bag/colour "shiny gold"}))))))

