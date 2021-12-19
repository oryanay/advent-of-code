(ns aoc.day9
  (:require [clojure.string :as str]
            [aoc.core :as core]))

(def sample "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn char->num [c] (- (int c) 48))
(defn parse-line [s] (mapv char->num s))
;; (parse-line "8767896789");; => [8 7 6 7 8 9 6 7 8 9]

(def sample-vec (->> sample
                     str/split-lines
                     (mapv parse-line)))

(defn adjacent [x y]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])
;; (adjacent 2 2);; => [[1 2] [3 2] [2 1] [2 3]]

(defn get-adjacent [m adj]
  (keep (fn [[x y]] (get-in m [y x])) adj))
;;(get-adjacent sample-vec (adjacent 2 2));; => (8 6 8 6)
;; (get-adjacent sample-vec (adjacent 0 0));; => (1 3)

(defn smallest? [n adj]
  (every? #(< n %) adj))

(defn find-low [m]
  (for [y (range (count m))
        x (range (count (first m)))
        :let [adj-points (adjacent x y)
              adj-vals (get-adjacent m adj-points)
              n (get-in m [y x])]
        :when (smallest? n adj-vals)]
    n))

(reduce + (map inc (find-low sample-vec)));; => 15

(def input-vec (into [] (core/read-file "resources/day9.txt" parse-line )))

(->>  input-vec
      find-low
      (map inc)
      (reduce +));; => 572


;; part 2 - recursion on all adjacent numbers until 9

;; (defn vec->map [v]
;;   (mapv #(hash-map :v % :set? false) v))

;; (defn matrix->map [m]
;;   (mapv vec->map m))
;; (matrix->map sample-vec)

(defn visited-matrix [m]
  (into [] (for [y (range (count m))]
           (into [] (for [x (range (count (first m)))]
                    false)))))

(visited-matrix sample-vec)

(defn visited?
  "Predicate which returns true if the point p has been visited already, false otherwise."
  [p coll]
  (some #(= % p) coll))
;; (visited? [1 1] [[1 2] [2 2] [1 1]]);; => true

(defn valid-adjacent [m [x y]]
  (let [neighbors (adjacent x y)]
    (keep (fn [[a b]] (when (> 9 (or (get-in m [b a]) 9)) [a b])) neighbors)))

;; (valid-adjacent sample-vec [0 0]);; => ([1 0] [0 1])
;; (valid-adjacent sample-vec [0 1]);; => ([0 0])

(defn find-low-points [m]
  (for [y (range (count m))
        x (range (count (first m)))
        :let [adj-points (adjacent x y)
              adj-vals (get-adjacent m adj-points)
              n (get-in m [y x])]
        :when (smallest? n adj-vals)]
    [x y]))
;; (find-low-points sample-vec);; => ([1 0] [9 0] [2 2] [6 4])

(defn graph-dfs
  "Traverses a graph in Depth First Search (DFS)"
  [graph p]
  (loop [stack   (vector p) ;; Use a stack to store nodes we need to explore
         visited []]        ;; A vector to store the sequence of visited nodes
    (if (empty? stack)      ;; Base case - return visited nodes if the stack is empty
      visited
      (let [p           (peek stack)
            neighbors   (valid-adjacent graph p)
            not-visited (filter (complement #(visited? % visited)) neighbors)
            new-stack   (into (pop stack) not-visited)]
        (if (visited? p visited)
          (recur new-stack visited)
          (recur new-stack (conj visited p)))))))
;; (graph-dfs sample-vec [0 0]);; => [[0 0] [0 1] [1 0]]

(defn find-basins [m]
  (let [points (find-low-points m)]
    (map #(graph-dfs m %) points)))

(->>  sample-vec
      find-basins
      (map count)
      sort
      (take-last 3)
      (apply *));; => 1134

(->>  input-vec
      find-basins
      (map count)
      sort
      (take-last 3)
      (apply *));; => 847044
