(ns aoc.day5
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [aoc.core :as core]))

(def sample "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn str->point [s]
  (->> s
       str/trim
       (#(str/split % #","))
       (mapv #(Integer/parseInt %))))
;; (str->point "0,9 ");; => (0 9)

(defn parse-line [s]
  (->> s
       (#(str/split % #"->"))
       (mapv str->point)))
;; (parse-line "0,9 -> 5,9");; => ((0 9) (5 9))

(defn streight-line? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

;; (streight-line? '((0 9) (5 9)));; => true

(defn rangev [[[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (for [y (range (min y1 y2) (inc (max y1 y2)))] (str x1 "-" y))
    (= y1 y2) (for [x (range (min x1 x2) (inc (max x1 x2)))] (str x "-" y1))
    :else []))

;; (rangev '((0 9) (5 9)));; => ("0-9" "1-9" "2-9" "3-9" "4-9" "5-9")

;; (rangev '((9 4) (3 4)));; => ("3-4" "4-4" "5-4" "6-4" "7-4" "8-4" "9-4")

(def sample-points (->> sample
                        str/split-lines
                        (mapv parse-line)
                        ;; (filter streight-line?)
                        (into [])))

;; (reduce (fn [mv mc] (map (fn [k v] (update mv k #(+ % v))))) ))

(defn board-xs [b]
  (map first (mapcat identity b)))

(defn board-ys [b]
  (map second (mapcat identity b)))

(defn minx [b]
  (apply min (board-xs b)))

(defn miny [b]
  (apply min (board-ys b)))

(defn maxx [b]
  (apply max (board-xs b)))

(defn maxy [b]
  (apply max (board-ys b)))

(->> sample-points
     (map rangev)
     flatten
     frequencies
     (filter (fn [[_ v]] (> v 1)))
     count)

(def input (core/read-file "resources/day5.txt" parse-line))

(->> input
     (map rangev)
     flatten
     frequencies
     (filter (fn [[_ v]] (> v 1)))
     count);; => 5306


;; part 2 diagonal lines

(defn diff [a b]
  (if (> a b) (- a b) (- b a)))

(defn sign [a b]
  (if (> a b) -1 1))

(defn diag-rangev [[[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (for [y (range (min y1 y2) (inc (max y1 y2)))] (str x1 "-" y))
    (= y1 y2) (for [x (range (min x1 x2) (inc (max x1 x2)))] (str x "-" y1))
    (= (diff x2 x1) (diff y2 y1)) (let [ysign (sign y1 y2) xsign (sign x1 x2)]
                                    (for [i (range (inc (diff x1 x2)))]
                                      (str (+ x1 (* i xsign)) "-" (+ y1 (* ysign i)))))
    :else []))

;; (diag-rangev '((9 7)  (7 9)));; => ("9-7" "8-8" "7-9")

(->> sample-points
     (map diag-rangev)
     flatten
     frequencies
     (filter (fn [[_ v]] (> v 1)))
     count);; => 12

(->> input
     (map diag-rangev)
     flatten
     frequencies
     (filter (fn [[_ v]] (> v 1)))
     count);; => 17787
