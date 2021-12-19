(ns aoc.day3
  (:require [aoc.core :as core]))

(defn parser [s]
  (vec (map #(if (= % \#) true false) s)))
;; (parser "..#..");; => [false false true false false]
(def input (vec (core/read-file "resources/day3-input.txt" parser)))
(def example (vec (map parser ["..##......." "#...#...#.." ".#....#..#." "..#.#...#.#" ".#...##..#." "..#.##....."
                               ".#.#.#....#" ".#........#" "#.##...#..." "#...##....#" ".#..#...#.#"])))

(defn step [[a b] [x y]]
  [(+ x a) (+ y b)])

(defn adjusted-step [s j p]
  (let [[x y] (step j p)]
    (if (>= x s) [(- x s) y] [x y])))

(defn slide
  "Given a matrix, an x vector step and a y vector step return how many trues in matrix.
  duplicate matrix to the 'right' if needed."
  [m x y]
  (let [xsize (count (first m))
        ysize (count m)
        steps (quot ysize y)]
    (for [[x y] (take steps (iterate (partial adjusted-step xsize [x y]) [0 0]))]
      (get-in m [y x]))
    ))

(count (filter true? (slide example 3 1)));; => 193

(for [[x y] [[1 1] [3 1] [5 1] [7 1] [1 2]]]
  (count (filter true? (slide example x y))));; => (2 7 3 4 2)

(for [[x y] [[1 1] [3 1] [5 1] [7 1] [1 2]]]
  (count (filter true? (slide input x y))));; => (57 193 64 55 35)
;;    (57 193 64 55 35)

(apply * '(57 193 64 55 35));; => 1355323200
