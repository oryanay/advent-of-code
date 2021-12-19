(ns aoc.day7
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(def sample [16,1,2,0,4,2,7,1,2,14])
(def input (mapv #(Integer/parseInt %) (first (core/read-file "resources/day7.txt" #(str/split % #",")))))
(def sorted-input (vec (sort input)))
(count input);; => 1000

(defn avg [coll] (/ (reduce + coll) (count coll)))
(defn sqr [n] (* n n))
(defn abs [n] (if (neg? n) (* n -1) n))
(defn exp [n] (reduce + (range (inc n))))

(defn distance
  "Return the abs distance from target"
  [coll a]
  (->> coll
       (map #(abs (- a %)))
       (reduce +)))
;; (distance sample 7);; => 53
;; (distance sample 6);; => 49
;; (distance sample 5);; => 45
;; (distance sample 4);; => 41
;; (distance sample 3);; => 39
;; (distance sample 2);; => 37
;; (distance sample 1);; => 41
;;
;;get the median
(get sorted-input 500);; => 376
(distance input 374);; => 352711
(distance input 376);; => 352707  <--- distance from median is the minimum
(distance input 379);; => 352713

(defn rss [coll a]
  (->> coll
       (map #(sqr (- a %)))
       (reduce +)))

(defn exp-dist [coll a]
  (->> coll
       (map #(exp (abs (- a %))))
       (reduce +)))

;; (rss sample 6);; => 303
;; (rss sample 5);; => 291
;; (rss sample 4);; => 299
;; (rss sample 3);; => 327
;; (rss sample 2);; => 375

(->> input avg float);; => 490.543
(filter #(= 486 %) input)
(exp-dist input 0)  ;; => 34047109
(exp-dist input 490);; => 95519693
(exp-dist input 491);; => 95519725 <--- distance from average is the minimum
(exp-dist input 489);; => 95520661
(exp-dist input 486);; => 95529567
