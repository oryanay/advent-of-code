(ns aoc.day1
  (:require [aoc.core :as core]))

(def sample [199 200 208 210 200 207 240 269 260 263])

(def input (core/read-file "resources/day1.txt" #(Integer/parseInt %)))

(->> input
     (partition 2 1)
     (map (fn [[a b]] (if (< 0 (- b a)) 1 0)))
     (apply +));; => 1448

;; 2nd part

(->> input
     (partition 3 1)
     (map #(apply + %))
     (partition 2 1)
     (map (fn [[a b]] (if (< 0 (- b a)) 1 0)))
     (apply +));; => 1471
