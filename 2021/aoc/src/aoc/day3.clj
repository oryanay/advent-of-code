(ns aoc.day3
  (:require [clojure.string :as str]
            [aoc.core :as core]))

(def sample "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(->> "00100" (mapv #(- (int %) 48)))

(def input (core/read-file "resources/day3.txt" #(vec (seq %))))

(defn transpose [m]
  (apply mapv vector m))
;; (transpose [[1 2] [3 4]]);; => [[1 3] [2 4]]

(defn flip-binary [coll]
  (map #(if (= \0 %) \1 \0) coll))

(->> sample
     str/split-lines
     (mapv seq)
     (mapv vec)
     transpose
     (map frequencies)
     (map #(sort-by val %))
     (map last)
     (map key)
     (#(vector % (flip-binary %)))
     (map #(apply str %))
     (map #(Integer/parseInt % 2))
     (apply *))

(->> input
     (mapv vec)
     transpose
     (map frequencies)
     (map #(sort-by val %))
     (map last)
     (map key)
     (#(vector % (flip-binary %)))
     (map #(apply str %))
     (map #(Integer/parseInt % 2))
     (apply *));; => 3901196


;; (bit-not 22)
;;(mapv (mapv #(- (int %) 48)))

;; part 2
;;
(def sample-mat
  (->> sample
       str/split-lines
       (mapv seq)
       (mapv vec)))

(defn compare-freq [m]
  (let [ones (get m \1)
        zeros (get m \0)]
    (cond
      (= ones zeros) \1
      (> ones zeros) \1
      :else \0)))

(defn flip-bit [b] (if (= b \1) \0 \1))
;; (compare-freq {\0 3 \1 3} \0)

(defn most-freq [mat i most?]
  (->> mat
       (map #(nth % i))
       frequencies
       (#(if most? (compare-freq %) (flip-bit (compare-freq % ))))))

;; (most-freq sample-mat 0 true)

(defn filter-nth [mat fr i]
    (filter #(= fr (nth % i)) mat))

;; (filter-nth sample-mat \0 1)

(defn find-rating [mat most?]
  (loop [m mat
         i 0]
    (if (= 1 (count m)) (first m)
      (let [fr (most-freq m i most?)]
        (recur (filter-nth m fr i) (inc i))))))

;;(Integer/parseInt (apply str (find-rating sample-mat)) 2)
(->> input
     (#(vector (find-rating %) (flip-binary (find-rating %))))
     (map #(apply str %))
     (map #(Integer/parseInt % 2))
     (apply *));; => 4022924

(find-rating sample-mat false)
(most-freq '([\0 \1 \1 \1 \1] [\0 \1 \0 \1 \0]) 2 false)
(->> '([\0 \1 \1 \1 \1] [\0 \1 \0 \1 \0])
     (map #(nth % 2))
     frequencies
     (sort-by val))

(->> input ;;sample-mat
     (#(vector (find-rating % true) (find-rating % false)))
     (map #(apply str %))
     (map #(Integer/parseInt % 2))
     (apply *));; => 4412188
