(ns aoc.day8
  (:require [clojure.string :as str]
            [clojure.set :as sets]
            [aoc.core :as core]
            [clojure.set :as set]))

(def sample "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(defn parse-group [s]
  (str/trim (nth (re-matches #"[\ \w+]+\|([\ \w+]+)" s) 1)))
;; (parse-group "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb");; => "gbdfcae bgc cg cgb"

(->> sample
     str/split-lines
     (map  #(nth (re-matches #"[\ \w+]+\|([\ \w+]+)" %) 1))
     (map #(str/trim %))
     (map #(str/split % #" "))
     flatten
     (map #(count %))
     (filter #(#{2 3 4 7} %))
     count
)

(def input (core/read-file "resources/day8.txt" parse-group))

(->> input
     (map #(str/split % #" "))
     flatten
     (map #(count %))
     (filter #(#{2 3 4 7} %))
     count
     )


;; 2nd part decode the numbers

(defn parse-line [s]
  (let [[_ a b] (map #(str/split % #" ") (re-matches #"([\ \w+]+)\| ([\ \w+]+)" s))]
    [a b]))

(defn filter-count [coll n] (filter #(= (count %) n) coll))
(defn get-set-by-size [coll n] (set (first (filter-count coll n))))
(defn minus [s coll] (map #(sets/difference s %) coll))

;; (defn filter-set [coll n] (set (first (filter-count coll n))))
;; (defn diff-of-one [s1 & s2] (first (filter-count (map #(sets/difference % (sets/union s2)) s1) 1)))

(defn decode [line]
  (let [[numbers coded] line
        one (get-set-by-size numbers 2)
        seven (get-set-by-size numbers 3)
        four (get-set-by-size numbers 4)
        eight (get-set-by-size numbers 7)
        sixes (map set (filter-count numbers 6))
        fives (map set (filter-count numbers 5))
        c (first (filter #(get seven (first %)) (minus eight sixes)))
        six (first (filter #(not (contains? % (first c))) sixes))
        five (first (filter #(not (contains? % (first c))) fives))
        nine (sets/union one five)
        zero (first (sets/difference (set sixes) (set [nine six])))
        two (first (keep #(when (= 1 (count (sets/intersection one %))) %) (filter #(not= five %) fives)))
        three (first (sets/difference (set fives) (set [two five])))
        decoder {one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9 zero 0}
        ]
    (map decoder (map set coded))
    ))

(->> sample str/split-lines
     (map parse-line)
     (map decode)
     (map #(apply str %))
     (map #(Integer/parseInt %))
     (reduce +)
     );; => 61229

(->> (core/read-file "resources/day8.txt" parse-line)
     (map decode)
     (map #(apply str %))
     (map #(Integer/parseInt %))
     (reduce +)
     );; => 1012089



;; aaaa    ....    aaaa    aaaa    ....
;;b    c  .    c  .    c  .    c  b    c
;;b    c  .    c  .    c  .    c  b    c
;; ....    ....    dddd    dddd    dddd
;;e    f  .    f  e    .  .    f  .    f
;;e    f  .    f  e    .  .    f  .    f
;; gggg    ....    gggg    gggg    ....

;;  5:      6:      7:      8:      9:
;; aaaa    aaaa    aaaa    aaaa    aaaa
;;b    .  b    .  .    c  b    c  b    c
;;b    .  b    .  .    c  b    c  b    c
;; dddd    dddd    ....    dddd    dddd
;;.    f  e    f  .    f  e    f  .    f
;;.    f  e    f  .    f  e    f  .    f
;; gggg    gggg    ....    gggg    gggg
