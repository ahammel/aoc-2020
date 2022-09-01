(ns ahammel.aoc-2020.day-8
  (:require [ahammel.aoc-2020.common :refer [->int only with-input]]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(defn execute
  [state [instruction value]]
  (case instruction
    "nop" (update state ::pointer inc)
    "jmp" (update state ::pointer #(+ value %))
    "acc" (-> state
              (update ::acc #(+ value %))
              (update ::pointer inc))))

(defn run
  [program]
  (loop [{::keys [pointer executed], :as state}
           {::pointer 0, ::executed #{}, ::acc 0}]
    (let [instruction (get program pointer)]
      (cond (contains? executed pointer) (assoc state ::halts? false)
            (nil? instruction) (assoc state ::halts? true)
            :else (recur (-> state
                             (update ::executed #(conj % pointer))
                             (execute instruction)))))))

(defn decorrupt
  [program pointer]
  (let [[instruction value] (get program pointer)]
    (case instruction
      "nop" (assoc program pointer ["jmp" value])
      "jmp" (assoc program pointer ["nop" value])
      "acc" nil)))

(defn parse
  [lines]
  (into []
        (comp (filter seq)
              (map (fn [line]
                     (let [[instruction value] (string/split line #" ")]
                       [instruction (->int value)]))))
        lines))

(def fixture
  ["nop +0"  ;
   "acc +1"  ;
   "jmp +4"  ;
   "acc +3"  ;
   "jmp -3"  ;
   "acc -99" ;
   "acc +1"  ;
   "jmp -4"  ;
   "acc +6"  ;
   ""])

(deftest day-8-test
  (is (= 5
         (-> (parse fixture)
             run
             ::acc)))
  (is (= 1594
         (with-input "day-8.txt"
           (fn [lines]
             (-> (parse lines)
                 run
                 ::acc)))))
  (is (= 8
         (let [program (parse fixture)]
           (->> (range 0 (count program))
                (eduction (keep #(decorrupt program %))
                          (map run)
                          (filter ::halts?)
                          (take 1))
                only
                ::acc))))
  (is (= 758
         (with-input "day-8.txt"
           (fn [lines]
             (let [program (parse lines)]
               (->> (range 0 (count program))
                    (eduction (keep #(decorrupt program %))
                              (map run)
                              (filter ::halts?)
                              (take 1))
                    only
                    ::acc)))))))
