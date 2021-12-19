(ns aoc.day11
  (:require [clojure.string :as str]))

(def sample "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(def input "6617113584
6544218638
5457331488
1135675587
1221353216
1811124378
1387864368
4427637262
6778645486
3682146745")

(defn char->int [c] ( - (int c) 48))

(defn map-each [f m]
  (mapv #(mapv f %) m))

(def sample-m (->> sample
     str/split-lines
     (mapv seq)
     (map-each char->int)))

(defn points [m]
  (into [] (for [y (range (count m))
                 x (range (count (first m)))]
             [y x])))
;; (points [[1 10] [2 9]]);; => [[0 0] [0 1] [1 0] [1 1]]

(defn adj [[y x]]
  (for [x1 (range (dec x) (+ x 2))
        y1 (range (dec y) (+ y 2))
        :when (not= [x1 y1] [x y])]
    [y1 x1]))
;; (adj [2 2]);; => ([1 1] [1 2] [1 3] [2 1] [2 3] [3 1] [3 2] [3 3])
;; (adj [0 0]);; => ([-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1])

(defn point-val [m [x y]] (get-in m [y x]))

(defn filter-points [m]
  (into [] (for [p (points m)
                 :when (< 9 (get-in m p))]
             p)))
;; (filter-points [[1 10 9] [2 10 9]]);; => [[0 1] [1 1]]

(defn inc-point [m [y x]]
  (if (and
       (< -1 x (count (first m)))
       (< -1 y (count m)))
    (update-in m [y x] inc)
    m))

(prn "-------------------")

(defn inc-adj [m p]
  (reduce (fn [v p] (inc-point v p)) m (adj p)))

(defn zero-points [m xs]
  (prn xs)
  (reduce (fn [v p] (assoc-in v p 0)) m xs))

;; (let [xs (filter-point m)
        ;; ys (mapcat adj xs)]
    ;; (map #(inc-point m %) ys))

(defn step
  "Returns a map with all numbers incresed, with numbers > 9 turn to zero.
  In case a point in matrix go over 9, also increase adjascent numbers."
  [m]
  (loop [xm (map-each inc m)
         xp (filter-points xm)]
    (if (empty? xp) xm
        (let [p (peek xp)
              new-map (inc-adj (zero-points xm xp) p)]
          (recur new-map (filterv not-empty (conj (pop xp) (filter-points new-map))))))))

(->> sample-m
     step
     step
     )
