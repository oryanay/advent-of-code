(ns aoc.day4
  (:require [aoc.core :as core]
            [clojure.string :as str]))


;; (into {} (map #(str/split % #":") (str/split "hgt:176cm\niyr:2013\nhcl:#fffffd ecl:amb\nbyr:2000\neyr:2034\ncid:89 pid:934693255" #"\s")))
(defn parser [s] (into {} (map #(str/split % #":") (str/split s #"\s"))))

(def input (core/read-file "resources/day4-input.txt" (fn [s] (str/split s #"\n\n+")) parser))
(def example "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
")

(defn present? [m]
  (every? (partial contains? m) ["hgt" "pid" "byr" "eyr" "iyr" "ecl" "hcl"]))

(def example-records (map parser (str/split example #"\n\n+")))
(map present? example-records )
(count (filter true? (map present? input)))

(defn validate [r]

  )

(first input);; => {"hgt" "176cm",
;;     "iyr" "2013",
;;     "hcl" "#fffffd",
;;     "ecl" "amb",
;;     "byr" "2000",
;;     "eyr" "2034",
;;     "cid" "89",
;;     "pid" "934693255"}


(defn height-valid? [s]
  (let [[_ height metric] (re-find #"([0-9]+)(cm|in)" s)]
    (cond
      (= metric "cm") (<= 150 (Integer/parseInt height) 193)
      (= metric "in") (<= 59 (Integer/parseInt height) 76)
      :else false)))

(defmulti valid? (fn [[k v]] k))
(defmethod valid? "hgt" [[_ value]] (height-valid? value))
(defmethod valid? "iyr" [[_ value]] (<= 2010 (Integer/parseInt value) 2020))
(defmethod valid? "eyr" [[_ value]] (<= 2020 (Integer/parseInt value) 2030))
(defmethod valid? "byr" [[_ value]] (<= 1920 (Integer/parseInt value) 2020))
(defmethod valid? "ecl" [[_ value]] (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} value))
(defmethod valid? "pid" [[_ value]] (re-matches #"^\d{9}$" value))
(defmethod valid? "hcl" [[_ value]] (re-matches #"^#[0-9a-f]{6}$" value))
(defmethod valid? :default [_] true) ; valid for rest cases

;; (every? valid? {"eyr" "1972" "cid" "100" "hcl" "#18171d" "ecl" "amb" "hgt" "170" "pid" "186cm" "iyr" "2018" "byr" "1926"})
;; (every? valid? {"pid" "087499704" "hgt" "74in" "ecl" "grn" "iyr" "2012" "eyr" "2030" "byr" "1980" "hcl" "#623a2f"})

(count (filter true? (map valid? input)))
(map valid? (take 1 input))

(count (filter true? (map #(and (present? %) (every? valid? %)) input)))
