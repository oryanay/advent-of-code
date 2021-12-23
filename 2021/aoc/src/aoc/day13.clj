(ns aoc.day13
  (:require [clojure.string :as str]
            [aoc.core :as core]))

(def sample-points "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0")

(def sample-fold "fold along y=7
fold along x=5")

(defn parse-point [s]
  (mapv #(Integer/parseInt %) (str/split s #",")))
;; (parse-point "0,14");; => [0 14]

(defn fold-diff [n f]
  (if (< f n) (- f (- n f)) n))
;; (fold-diff 10 5)

(defn print-points [coll]
  (print "\n")
  (let [mx (apply max (map first coll))
        my (apply max (map second coll))
        p (set coll)]
               (for [y (range (inc my))
                     x (range (inc mx))]
                 (cond
                   (contains? p [x y]) (print "#" (if (= x mx) "\n" ""))
                   :else (print "." (if (= x mx) "\n" "")))
                 )))
;; (print-points [[0 0] [1 1]])


(->> sample-points
     str/split-lines
     (map parse-point)
     (filterv (fn [[x y]] (not= y 7)))
     (map #(vector (first %) (fold-diff (second %) 7)))
     set
     count
  ;;   print-points
);; => 17


(def input (-> "resources/day13.txt"
               (core/read-file parse-point)))


(->> input
     (map #(vector (fold-diff (first %) 655) (second %)))
     set
     count
);; => 942;; => 1027

;;2nd part finish the fold

(def input-fold [[655 447] [327 223] [163 111] [81 55] [40 27] [999 13] [999 6]])

(->>
 (reduce
  (fn [v [x y]] (set (map #(vector (fold-diff (first %) x) (fold-diff (second %) y)) v)))
  input
  [[655 447] [327 223] [163 111] [81 55] [40 27] [999 13] [999 6]])
 print-points)
;; . . # # . # # # # . . # # . . # . . # . . # # . . # # # . . # # # . . # # # .
;; . . . # . . . . # . # . . # . # . . # . # . . # . # . . # . # . . # . # . . #
;; . . . # . . . # . . # . . . . # . . # . # . . # . # . . # . # . . # . # # # .
;; . . . # . . # . . . # . # # . # . . # . # # # # . # # # . . # # # . . # . . #
;; # . . # . # . . . . # . . # . # . . # . # . . # . # . . . . # . # . . # . . #
;; . # # . . # # # # . . # # # . . # # . . # . . # . # . . . . # . . # . # # # .
