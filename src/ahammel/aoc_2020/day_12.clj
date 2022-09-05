(ns ahammel.aoc-2020.day-12
  (:require [ahammel.aoc-2020.common :refer
             [->int point+ manhattan-distance with-input]]
            [clojure.test :refer [deftest is]]))

(defn move
  [point direction magnitude]
  (case direction
    ::north (point+ point [magnitude 0])
    ::south (point+ point [(- magnitude) 0])
    ::east (point+ point [0 magnitude])
    ::west (point+ point [0 (- magnitude)])))

(def right
  {[::north 90] ::east,
   [::east 90] ::south,
   [::south 90] ::west,
   [::west 90] ::north,
   [::north 180] ::south,
   [::east 180] ::west,
   [::south 180] ::north,
   [::west 180] ::east,
   [::north 270] ::west,
   [::east 270] ::north,
   [::south 270] ::east,
   [::west 270] ::south})

(defn left [[heading degrees]] (right [heading (- 360 degrees)]))

(defn move-to-waypoint
  [position waypoint n]
  (apply point+ position (repeat n waypoint)))

(defn rotate-waypoint
  [[lat lon] direction degrees]
  (case [direction degrees]
    [::right 90] [(- lon) lat]
    [::right 180] [(- lat) (- lon)]
    [::right 270] [lon (- lat)]
    [::left 90] [lon (- lat)]
    [::left 180] [(- lat) (- lon)]
    [::left 270] [(- lon) lat]))

(defn navigate
  ([] (navigate {}))
  ([{::keys [heading position], :or {heading ::east, position [0 0]}}]
   {::heading heading, ::position position})
  ([{::keys [heading position], :or {heading ::east, position [0 0]}}
    [action magnitude]]
   (case action
     \N {::heading heading, ::position (move position ::north magnitude)}
     \S {::heading heading, ::position (move position ::south magnitude)}
     \E {::heading heading, ::position (move position ::east magnitude)}
     \W {::heading heading, ::position (move position ::west magnitude)}
     \F {::heading heading, ::position (move position heading magnitude)}
     \R {::heading (right [heading magnitude]), ::position position}
     \L {::heading (left [heading magnitude]), ::position position})))

(defn navigate-waypoint
  ([] (navigate-waypoint {}))
  ([{::keys [waypoint position], :or {waypoint [1 10], position [0 0]}}]
   {::waypoint waypoint, ::position position})
  ([{::keys [waypoint position], :or {waypoint [1 10], position [0 0]}}
    [action magnitude]]
   (case action
     \N {::waypoint (move waypoint ::north magnitude), ::position position}
     \S {::waypoint (move waypoint ::south magnitude), ::position position}
     \E {::waypoint (move waypoint ::east magnitude), ::position position}
     \W {::waypoint (move waypoint ::west magnitude), ::position position}
     \F {::waypoint waypoint,
         ::position (move-to-waypoint position waypoint magnitude)}
     \R {::waypoint (rotate-waypoint waypoint ::right magnitude),
         ::position position}
     \L {::waypoint (rotate-waypoint waypoint ::left magnitude),
         ::position position})))

(defn parse-line
  [line]
  [(first line)
   (->> (rest line)
        (apply str)
        ->int)])

(def fixture ["F10" "N3" "F7" "R90" "F11" ""])

(deftest day-12-test
  (is (= 25
         (->> fixture
              (transduce (comp (filter seq) (map parse-line)) navigate)
              ::position
              manhattan-distance
              (apply +))))
  (is (= 845
         (with-input "day-12.txt"
           (fn [lines]
             (->> lines
                  (transduce (comp (filter seq) (map parse-line)) navigate)
                  ::position
                  manhattan-distance
                  (apply +))))))
  (is (= 286
         (->> fixture
              (transduce (comp (filter seq) (map parse-line)) navigate-waypoint)
              ::position
              manhattan-distance
              (apply +))))
  (is (= 27016
         (with-input "day-12.txt"
           (fn [lines]
             (->> lines
                  (transduce (comp (filter seq) (map parse-line))
                             navigate-waypoint)
                  ::position
                  manhattan-distance
                  (apply +)))))))
