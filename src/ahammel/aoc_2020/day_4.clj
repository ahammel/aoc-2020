(ns ahammel.aoc-2020.day-4
  (:require [ahammel.aoc-2020.common :refer [->int with-input]]
            [clojure.set :refer [difference]]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(defn ->height
  [s]
  (let [[matches? height unit] (re-matches #"(\d+)(cm|in)" s)]
    (when matches? (when-let [h (->int height)] [h unit]))))

(s/def :passport/byr #(when-let [byr (->int %)] (<= 1920 byr 2002)))
(s/def :passport/iyr #(when-let [iyr (->int %)] (<= 2010 iyr 2020)))
(s/def :passport/eyr #(when-let [eyr (->int %)] (<= 2020 eyr 2030)))
(s/def :passport/hgt
  #(when-let [[height unit] (->height %)]
     (case unit
       "cm" (<= 150 height 193)
       "in" (<= 59 height 76))))
(s/def :passport/hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def :passport/ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def :passport/pid #(re-matches #"[0-9]{9}" %))

(s/def ::passport
  (s/keys :req [:passport/byr :passport/iyr :passport/eyr :passport/hgt
                :passport/hcl :passport/ecl :passport/pid]
          :opt [:pasport/cid]))

(def required-fields (let [[_ _ fields] (s/form ::passport)] (set fields)))

(defn missing-fields
  [passport]
  (seq (difference required-fields (set (keys passport)))))

(defn parse
  [lines]
  (let [field->kv (fn
                    ([field-string]
                     (let [[field value] (string/split field-string #":")]
                       [(keyword "passport" field) value])))]
    (->> lines
         (eduction (map string/trim)
                   (partition-by empty?)
                   (filter #(not= [""] %))
                   (map (fn [lines] (mapcat #(string/split % #" ") lines)))
                   (map (fn [field-strings]
                          (into {} (map field->kv) field-strings)))))))


(def fixture-1
  ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"      ;
   "byr:1937 iyr:2017 cid:147 hgt:183cm"             ;
   ""                                                ;
   "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884" ;
   "hcl:#cfa07d byr:1929"                            ;
   ""                                                ;
   "hcl:#ae17e1 iyr:2013"                            ;
   "eyr:2024"                                        ;
   "ecl:brn pid:760753108 byr:1931"                  ;
   "hgt:179cm"                                       ;
   ""                                                ;
   "hcl:#cfa07d eyr:2025 pid:166559648"              ;
   "iyr:2011 ecl:brn hgt:59in"                       ;
   "" ""])

(def invalid-passports
  ["eyr:1972 cid:100"                                          ;
   "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"   ;
   ""                                                          ;
   "iyr:2019"                                                  ;
   "hcl:#602927 eyr:1967 hgt:170cm"                            ;
   "ecl:grn pid:012533040 byr:1946"                            ;
   ""                                                          ;
   "hcl:dab227 iyr:2012"                                       ;
   "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277" ;
   ""                                                          ;
   "hgt:59cm ecl:zzz"                                          ;
   "eyr:2038 hcl:74454a iyr:2023"                              ;
   "pid:3556412378 byr:2007"                                   ;
  ])

(def valid-passports
  [" pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"               ;
   "hcl:#623a2f"                                                              ;
   ""                                                                         ;
   "eyr:2029 ecl:blu cid:129 byr:1989"                                        ;
   "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"                             ;
   ""                                                                         ;
   "hcl:#888785"                                                              ;
   "hgt:164cm byr:2001 iyr:2015 cid:88"                                       ;
   "pid:545766238 ecl:hzl"                                                    ;
   "eyr:2022"                                                                 ;
   ""                                                                         ;
   "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719  " ;
  ])

(deftest day-4-test
  (is (= 2
         (->> (parse fixture-1)
              (remove missing-fields)
              count)))
  (is (= 219
         (with-input "day-4.txt"
           (fn [lines]
             (->> (parse lines)
                  (remove missing-fields)
                  count)))))
  (is (empty? (->> (parse invalid-passports)
                   (filter #(s/valid? ::passport %)))))
  (is (= 4
         (->> (parse valid-passports)
              (filter #(s/valid? ::passport %))
              count)))
  (is (= 127
         (with-input "day-4.txt"
           (fn [lines]
             (->> (parse lines)
                  (filter #(s/valid? ::passport %))
                  count))))))
