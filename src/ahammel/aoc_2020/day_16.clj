(ns ahammel.aoc-2020.day-16
  (:require [ahammel.aoc-2020.common :refer [->int only with-input]]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(defn rule->fn
  [{:rule/keys [rule-name ranges]}]
  (fn [n]
    (let [applies (fn [{:rule/keys [from to]}] (when (<= from n to) rule-name))]
      (first (keep applies ranges)))))

(defn rules->fn
  [rules]
  (fn [n]
    (let [matching-rules (into #{} (comp (map rule->fn) (keep #(% n))) rules)]
      (when (seq matching-rules) matching-rules))))

(defn infer-fields
  ([field-sets] (infer-fields field-sets {}))
  ([field-sets field-map]
   (let [enumerated (map-indexed vector field-sets)
         singletons (keep (fn [[ix s]] (when (= 1 (count s)) [ix (only s)]))
                          enumerated)
         inferred (into #{} (map second) singletons)
         field-map' (into field-map singletons)
         field-sets' (map (fn [[ix s]]
                            (if (contains? field-map' ix)
                              #{}
                              (set/difference s inferred)))
                       enumerated)]
     (if (empty? inferred) field-map (recur field-sets' field-map')))))

(defn parse
  [lines]
  (let [line->ints (fn [line] (map ->int (string/split line #",")))
        parse-rule
          (fn [line]
            (let [[rule-name ranges] (string/split line #": ")]
              {:rule/rule-name rule-name,
               :rule/ranges (into []
                                  (comp (map #(string/split % #"-"))
                                        (map (fn [[from to]]
                                               {:rule/from (->int from),
                                                :rule/to (->int to)})))
                                  (string/split ranges #" or "))}))
        [rules my-ticket nearby-tickets] (->> lines
                                              (partition-by empty?)
                                              (remove #(= [""] %)))]
    {::rules (map parse-rule rules),
     ::my-ticket (line->ints (second my-ticket)),
     ::nearby-tickets (map line->ints (rest nearby-tickets))}))


(def fixture-1
  ["class: 1-3 or 5-7"    ;
   "row: 6-11 or 33-44"   ;
   "seat: 13-40 or 45-50" ;
   ""                     ;
   "your ticket:"         ;
   "7,1,14"               ;
   ""                     ;
   "nearby tickets:"      ;
   "7,3,47"               ;
   "40,4,50"              ;
   "55,2,20"              ;
   "38,6,12"              ;
   ""                     ;
   ""])

(def fixture-2
  ["class: 0-1 or 4-19"  ;
   "row: 0-5 or 8-19"    ;
   "seat: 0-13 or 16-19" ;
   ""                    ;
   "your ticket:"        ;
   "11,12,13"            ;
   ""                    ;
   "nearby tickets:"     ;
   "3,9,18"              ;
   "15,1,5"              ;
   "5,14,9"              ;
   ""])

(deftest day-16-test
  (is (= 71
         (let [{::keys [rules _ nearby-tickets]} (parse fixture-1)
               f (rules->fn rules)]
           (transduce (comp (mapcat identity) (remove f)) + nearby-tickets))))
  (is (= 27911
         (with-input "day-16.txt"
           (fn [lines]
             (let [{::keys [rules _ nearby-tickets]} (parse lines)
                   f (rules->fn rules)]
               (transduce (comp (mapcat identity) (remove f))
                          +
                          nearby-tickets))))))
  (is (= {0 "row", 1 "class", 2 "seat"}
         (let [{::keys [rules _ nearby-tickets]} (parse fixture-2)
               f (rules->fn rules)]
           (->> nearby-tickets
                (keep (fn [ticket]
                        (let [classes (map f ticket)]
                          (when (every? seq classes) classes))))
                (reduce (fn [acc ticket] (map set/intersection acc ticket)))
                infer-fields))))
  (is (= 737176602479
         (with-input "day-16.txt"
           (fn [lines]
             (let [{::keys [rules my-ticket nearby-tickets]} (parse lines)
                   f (rules->fn rules)]
               (->> nearby-tickets
                    (keep (fn [ticket]
                            (let [classes (map f ticket)]
                              (when (every? seq classes) classes))))
                    (reduce (fn [acc ticket] (map set/intersection acc ticket)))
                    infer-fields
                    (keep (fn [[ix field]]
                            (when (string/starts-with? field "departure") ix)))
                    (map #(get (vec my-ticket) %))
                    (reduce *))))))))

