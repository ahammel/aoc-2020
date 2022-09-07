(ns ahammel.aoc-2020.day-14
  (:require [ahammel.aoc-2020.common :refer [->int with-input]]
            [clojure.test :refer [deftest is]]))

(defn set-bit
  [n bit]
  (bit-xor n (bit-and (bit-xor -1 n) (bit-shift-left 1 bit))))

(defn unset-bit
  [n bit]
  (bit-xor n (bit-and (bit-xor 0 n) (bit-shift-left 1 bit))))

(defn float-bit
  [n bit]
  (let [f (juxt #(set-bit % bit) #(unset-bit % bit))] (f n)))

(defn mask-value
  [n mask]
  (let [apply-mask-element (fn [n' [ix instruction]]
                             (case instruction
                               \1 (set-bit n' ix)
                               \0 (unset-bit n' ix)
                               \X n'))]
    (reduce apply-mask-element n mask)))

(defn mask-address
  [address mask]
  (let [address' (transduce (keep (fn [[bit instruction]]
                                    (when (= \1 instruction) bit)))
                            (fn ([] address) ([n] n) ([n bit] (set-bit n bit)))
                            mask)]
    (transduce (keep (fn [[bit instruction]] (when (= \X instruction) bit)))
               (fn
                 ([] [address'])
                 ([addresses] addresses)
                 ([addresses bit] (mapcat #(float-bit % bit) addresses)))
               mask)))

(defn run-bitmask
  [program]
  (let [execute
          (fn [state {::keys [mask register value], :as instruction}]
            (cond mask (assoc state ::mask mask)
                  (and register value) (assoc-in state
                                         [::memory register]
                                         (mask-value value (::mask state)))
                  :else (throw (Exception. (str "Unprocessible instruction: "
                                                instruction)))))]
    (reduce execute {::mask nil, ::memory {}} program)))

(defn run-mad
  [program]
  (let [execute
          (fn [state {::keys [mask register value], :as instruction}]
            (cond mask (assoc state ::mask mask)
                  (and register value)
                    (assoc state
                      ::memory (into (::memory state)
                                     (map #(vector % value))
                                     (mask-address register (::mask state))))
                  :else (throw (Exception. (str "Unprocessible instruction: "
                                                instruction)))))]
    (reduce execute {::mask nil, ::memory {}} program)))

(defn parse
  [lines]
  (let [parse-mask (fn [line]
                     (let [[matches? mask-string]
                             (re-matches #"^mask = ([X10]+)$" line)
                           mask (->> mask-string
                                     reverse
                                     (into [] (map-indexed vector)))]
                       (when matches? {::mask mask})))
        parse-write (fn [line]
                      (let [[matches? register value]
                              (re-matches #"^mem\[(\d+)\] = (\d+)" line)]
                        (when matches?
                          {::register (->int register),
                           ::value (->int value)})))]
    (map #(or (parse-mask %) (parse-write %)) lines)))

(def fixture-1
  ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" ;
   "mem[8] = 11"                                 ;
   "mem[7] = 101"                                ;
   "mem[8] = 0"                                  ;
  ])

(def fixture-2
  ["mask = 000000000000000000000000000000X1001X" ;
   "mem[42] = 100"                               ;
   "mask = 00000000000000000000000000000000X0XX" ;
   "mem[26] = 1"                                 ;
  ])

(deftest day-14-test
  (is (= 165
         (->> (run-bitmask (parse fixture-1))
              ::memory
              vals
              (reduce +))))
  (is (= 10452688630537
         (with-input "day-14.txt"
           (fn [lines]
             (->> lines
                  (filter seq)
                  parse
                  run-bitmask
                  ::memory
                  vals
                  (reduce +))))))
  (is (= 208
         (->> (run-mad (parse fixture-2))
              ::memory
              vals
              (reduce +))))
  (is (= 2881082759597
         (with-input "day-14.txt"
           (fn [lines]
             (->> lines
                  (filter seq)
                  parse
                  run-mad
                  ::memory
                  vals
                  (reduce +)))))))

