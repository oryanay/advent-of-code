(ns aoc.day9
  (:require [clojure.string :as str]
            [aoc.core :as core]))

(def sample ".#.
..#
###")

(def input "#.#..###
.#....##
.###...#
..####..
....###.
##.#.#.#
..#..##.
#.....##")

(defn parse-line [s]
  (let [pm {\. 0 \# 1}]
    (mapv #(pm %) s)))

;; (parse-line "..#");; => [0 0 1]

(def sample-matrix
  (->> sample
       str/split-lines
       (mapv parse-line)))

;; [[0 1 0] [0 0 1] [1 1 1]]

(def input-matrix
  (->> input
       str/split-lines
       (mapv parse-line)))

(defn neighbors [z y x]
  (for [a [(dec x) x (inc x)]
        b [(dec y) y (inc y)]
        c [(dec z) z (inc z)]
        :when (or (not= a x) (not= b y) (not= c z))]
    [c b a]))
;; (neighbors 0 1 1)
(neighbors -1 0 0)

(defn is-active? [mat z y x]
  (let [n (neighbors z y x)
        m (keep #(get-in mat %) n)
        active-neighbors (reduce + m)
        status (get-in mat [z y x] 0)]
    (cond
      (and (= 0 status) (= 3 active-neighbors)) 1
      (and (= 1 status) (<= 2 active-neighbors 3)) 1
      :else 0)))

(is-active? [sample-matrix] -1 1 0)

(defn cycle-step [mat]
  (into [] (for [z (range -1 (inc (count mat)))]
             (into [] (for [y (range -1 (inc (count (mat 0))))]
                        (into [] (for [x (range -1 (inc (count (get-in mat [0 0]))))]
                                   (is-active? mat z y x))))))))

(cycle-step [sample-matrix])
(->> [sample-matrix]
     (iterate cycle-step)
     (take 7)
     last
     flatten
     (reduce +)
     )

(take 2 (iterate cycle-step [sample-matrix]))

(->> [input-matrix]
     cycle-step
     cycle-step
     cycle-step
     cycle-step
     cycle-step
     cycle-step
     flatten
     (apply +))


;; second part - n dimension hyper cube
(defn dim-neighbors [w z y x]
  (for [a [(dec x) x (inc x)]
        b [(dec y) y (inc y)]
        c [(dec z) z (inc z)]
        d [(dec w) w (inc w)]
        :when (or (not= a x) (not= b y) (not= c z) (not= d w))]
    [d c b a]))
;; (neighbors 0 1 1)

(defn dim-is-active? [mat w z y x]
  (let [n (dim-neighbors w z y x)
        m (keep #(get-in mat %) n)
        active-neighbors (reduce + m)
        status (get-in mat [w z y x] 0)]
    (cond
      (and (= 0 status) (= 3 active-neighbors)) 1
      (and (= 1 status) (<= 2 active-neighbors 3)) 1
      :else 0)))

(dim-is-active? [[sample-matrix]] -1 1 0 0)

(defn dim-cycle-step [mat]
  (into [] (for [w (range -1 (inc (count mat)))]
        (into [] (for [z (range -1 (inc (count (mat 0))))]
             (into [] (for [y (range -1 (inc (count (get-in mat [0 0 0]))))]
                        (into [] (for [x (range -1 (inc (count (get-in mat [0 0 0]))))]
                                   (dim-is-active? mat w z y x))))))))))


(->> [[input-matrix]]
     dim-cycle-step
     dim-cycle-step
     dim-cycle-step
     dim-cycle-step
     dim-cycle-step
     dim-cycle-step
     flatten
     (apply +))

;; attempt to do this DRY

(defn matrix
  "Returning a collection of coordinates, reprisenting a matrix indices.
  Input the range for each index"
  [[f & r]]
  (if (some? r)
    (for [d (range f)
          ds (matrix r)]
      (into [d] ds))
    (map vector (range f))))
;; (matrix [1 3 3])

(defn get-neighbors [[f & r]]
  (if (some? r)
    (for [d (range (dec f) (+ f 2))
          ds (get-neighbors r)]
      (into [d] ds))
    (map vector (range (dec f) (+ f 2)))))
;; (get-neighbors [0 0 0])

(defn active-neighbors [mat coordinate]
  (->> coordinate
       get-neighbors
       (filter #(not= coordinate %))
       (keep #(get-in mat %))
       flatten
       (apply +)))
;; (active-neighbors [[sample-matrix]] [0 0 1 2])

(defn active? [mat coordinate]
  (let [stat (get-in mat coordinate 0)]
    (cond
      (and (= 0 stat) (= 3 (active-neighbors mat coordinate))) 1
      (and (= 1 stat) (<= 2 (active-neighbors mat coordinate) 3)) 1
      :else 0)))
;; (active? [sample-matrix] [0 1 1])

(defn dims [mat]
  (loop [r [] m mat]
    (if (number? m) r
        (recur (conj r (count m)) (first m)))))
(dims sample-matrix)

(defn do-cycle [mat]
  (->> mat
       dims
       matrix
       (into (get-neighbors [0 0]))))
       ;; (map #(active? mat %))))
(->> sample-matrix dims matrix
     (map into (get-neighbors [0 0])))
(do-cycle sample-matrix)
