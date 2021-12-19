(ns report-repair
  (:require [clojure.string :as str]))

;; get input from file
(defn file->lines [file]
  (str/split-lines (slurp file)))

(defn str->int [s]
  (Integer/parseInt s))

(defn get-input []
  (map str->int (file->lines "resources/input.txt")))

(def sample '(1721 979 366 299 675 1456))
(def input-set (set (get-input)))
(def input-list (get-input))

;; brute force solution
(defn search [n x]
  (let [xs (vec (sort x))
        size (count xs)]
    (loop [i 0 j 1]
      (let [x (get xs i)
            y (get xs j)
            sum (+ x y)]
      ;; (prn i j x y sum)
      (cond
        (= sum n) [x y]
        (<= j i) (recur i (inc j))
        (or (<= size j) (< n sum)) (recur (inc i) (+ i 2))
        (<= size i) nil
        :else (recur i (inc j)))))))

;; (search 2020 sample)
;; (input-set (- 2020 833))
;; (search 2020 (get-input));; => [833 1187]
;; (apply * (search 2020 (get-input)));; => 988771

;; hash-set lookup for complemantry number
(defn lookup [n x]
  (let [xs (set x)]
    (loop [[f & r] x]
      (cond
        (xs (- n f)) [f (- n f)]
        (empty? r) nil
        :else (recur r)))))

;; (lookup 2020 sample)
;; (lookup 2020 input-list)

(defn three-entry-sum [n x]
  (loop [[f & r] x]
      (if (empty? r) nil
        ;; (>= (+ f (last r)) n) (recur (butlast r))
        (if-let [[a b]  (lookup (- n f) r)]
                [f a b]
                (recur r)))))

;; (three-entry-sum 2020 sample)
;; (three-entry-sum 2020 input-list)
;; (apply * (three-entry-sum 2020 input-list));; => 171933104
