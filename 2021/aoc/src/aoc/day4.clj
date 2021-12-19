(ns aoc.day4
  (:require [clojure.string :as str]
            [aoc.core :as core]))

(def rands [46,79,77,45,57,34,44,13,32,88,86,
            82,91,97,89,1,48,31,18,10,55,74,
            24,11,80,78,28,37,47,17,21,61,26,
            85,99,96,23,70,3,54,5,41,50,63,14,
            64,42,36,95,52,76,68,29,9,98,35,84,
            83,71,49,73,58,56,66,92,30,51,20,
            81,69,65,15,6,16,39,43,67,7,59,40,
            60,4,90,72,22,0,93,94,38,53,87,27,
            12,2,25,19,8,62,33,75])

(def sample-rands [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1])
(def sample "22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(defn create-board [s]
  (->> s
       (map str/trim)
       (map #(str/split % #"[ ]+"))
       (mapv #(mapv (fn [n] (Integer/parseInt n)) %))))

(create-board ["22 13 17 11  0"
     " 8  2 23  4 24"
     "21  9 14 16  7"
     " 6 10  3 18  5"
     " 1 12 20 15 19"]);; => [[22 13 17 11 0] [8 2 23 4 24] [21 9 14 16 7] [6 10 3 18 5] [1 12 20 15 19]]


(defn create-index [board]
  (into {}
        (map (fn [[a b]] (vector (get-in board [b a]) {:x a :y b :check false}))
             (for [x (range 5)
                   y (range 5)]
               [x y]))))
;; (prn (create-index [[22 13 17 11 0] [8 2 23 4 24] [21 9 14 16 7] [6 10 3 18 5] [1 12 20 15 19]]))
(prn (create-index (create-board ["22 13 17 11  0"
               " 8  2 23  4 24"
               "21  9 14 16  7"
               " 6 10  3 18  5"
               " 1 12 20 15 19"])))

(defn mark-board [n board]
  (if-let [{x :x y :y} (get board n)]
    (->  board
         (assoc-in [n :check] true)
         (update-in [:y y] inc)
         (update-in [:x x] inc))
    board))

(defn winner? [board]
  (let [xm (vals (:x board))
        ym (vals (:y board))]
    (or
     (some #(= 5 %) xm)
     (some #(= 5 %) ym))))

(def sample-boards (->> sample
                        (#(str/split % #"\n\n"))
                        (map #(str/split-lines %))
                        (map create-board)
                        (map create-index)
                        (map #(assoc % :x (zipmap (range 0 5) (repeat 5 0))))
                        (map #(assoc % :y (zipmap (range 0 5) (repeat 5 0))))
                        ))

(defn unchecked-vals [board]
  (apply + (keys (filter (fn [[k v]] (and (number? k) (not (:check v)))) board))))

(defn find-winer [boards xn]
  (loop [m boards
         n xn
         lw nil]
    (if (or (some winner? m) (empty? n)) (* lw (unchecked-vals (some #(when (winner? %) %)  m)))
        (recur (map #(mark-board (first n) %) m) (rest n) (first n)))))


(find-winer sample-boards sample-rands)

(def input (core/read-file "resources/day4.txt" #(str/split % #"\n\n") #(str/split-lines %)))

(def input-boards (->> input
                        (map create-board)
                        (map create-index)
                        (map #(assoc % :x (zipmap (range 0 5) (repeat 5 0))))
                        (map #(assoc % :y (zipmap (range 0 5) (repeat 5 0))))
                        ))

(find-winer input-boards rands);; => 22680

;; 2nd part
;;

(defn map-boards [n boards]
  (map #(mark-board n %) boards))

(defn find-loser [boards xn]
   (loop [m boards
         n xn
         lw nil]
     (prn n)
     (cond
       (and (empty? (rest m)) (winner? (first m))) (* lw (unchecked-vals (first m)))
       (and (empty? (rest m)) (not (winner? (first m)))) (recur (map-boards (first n) m) (rest n) (first n))
       (some winner? m) (recur (map-boards (first n) (filter #(not (winner? %)) m)) (rest n) (first n))
       :else (recur (map-boards (first n) m) (rest n) (first n)))))

(find-loser sample-boards sample-rands)
(find-loser input-boards rands);; => 16168
