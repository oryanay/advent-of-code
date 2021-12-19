(ns aoc.day5
  (:require [aoc.core :as core]))

(def ex1 "BFFFBBFRRR")

(split-at 7 (seq ex1));; => (\B \F \F \F \B \B \F)

(defn binary-search [[from to] c]
  (let [diff (- to from)]
    (if (or (= c \F) (= c \L))
      [from (+ from (quot diff 2))]
      [(+ 1 from (quot diff 2)) to])))

(defn search-seat [s]
  (let [[row column] (split-at 7 s)]
    (+ (* 8 (first (reduce binary-search [0 127] row)))
       (first (reduce binary-search [0 7] column)))))

(apply max (map search-seat (core/read-file "resources/day5-input.txt" identity)));; => 919;; => 919

(reduce
 (fn [v c] (if (< 1 (- c v)) (reduced (dec c)) c))
 (sort (map search-seat
            (core/read-file "resources/day5-input.txt" identity))));; => 642

(search-seat ex1)
(search-seat "FFFBBBFRRR")
(search-seat "BBFFBBFRLL")

(binary-search [0 63] \B)
(binary-search [64 127] \F)
(reduce binary-search [0 127] '(\B \F \F \F \B \B \F))
