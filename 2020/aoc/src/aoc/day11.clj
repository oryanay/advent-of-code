(ns aoc.day11
  (:require [clojure.string :as str]
            [aoc.core :as core]))

(def sample "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(defn parse-line [s] (vec s))

(def sample-db (->> sample str/split-lines (mapv parse-line)))

(defn neighbors [x y]
  (for [x1 [-1 0 1]
        y1 [-1 0 1]
        :let [xd (+ x x1)
              yd (+ y y1)]
        :when (and (not= 0 x1 y1) (<= 0 xd) (<= 0 yd))]
    [xd yd]))

(defn empty-seat? [mat x y]
  (= \L (get-in mat [y x])))

(defn occupied? [mat x y]
  (= \# (get-in mat [y x])))

(defn no-adj-occupied? [mat x y]
  (not (some identity (map (fn [[a b]] (occupied? mat a b)) (neighbors x y)))))

(defn at-least-four-adj-occupied? [mat x y]
  (<= 4 (apply + (map (fn [[x y]] (if (occupied? mat x y) 1 0)) (neighbors x y)))))

(defn rule [mat x y]
  (cond
    (= (get-in mat [y x]) \.) \.
    (and (empty-seat? mat x y) (no-adj-occupied? mat x y)) \#
    (and (occupied? mat x y) (at-least-four-adj-occupied? mat x y)) \L
    :else (get-in mat [y x])))

;; (some identity (map (fn [[a b]] (occupied? sample-db a b)) (neighbors 0 0)))
;; (no-adj-occupied? sample 0 0)
;; (rule sample-db 0 0)

;; (rule sample-db 0 1)

(let [ysize (count sample-db)
      xsize (count (first sample-db))]
  [xsize ysize])

(defn step [mat]
  (into [] (for [y (range (count mat))]
             (vec (map-indexed (fn [i _] (rule mat i y)) (get mat y))))))

(defn run [mat]
  (loop [m mat
         p []]
    (if (= m p) m (recur (step m) m))))

(->> sample-db
     run
     flatten
     frequencies)

(def input (into [] (core/read-file "resources/day10-input.txt" parse-line)))
input

(->> input
     run
     flatten
     frequencies)
