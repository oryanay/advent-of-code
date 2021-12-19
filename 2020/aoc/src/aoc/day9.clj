(ns aoc.day9
  (:require [aoc.core :as core]))


(def sample-db [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])
;; [35 20 15 25 47] 40 (15 + 25)
;; [20 15 25 47 40] 62 (47 + 15)
;; [15 25 47 40 62] 55 (40 + 15)

(def input (core/read-file "resources/day9-input.txt" #(Long/parseLong %)))

(defn find-sum [v n]
  (when (empty? (for [x v
                      y v
                      :when (and (not= x y) (= (+ x y) n) (> y x))]
                  [x y])) n))

(find-sum (subvec sample-db 0 5) 40)

;; (keep (fn [p] (find-sum (take 5 p) (nth p 5))) (partition 6 1 sample-db));; => (127)

;; (keep (fn [p] (find-sum (take 25 p) (nth p 25))) (partition 26 1 input));; => (31161678)

(defn find-cont [v n]
  (loop [search []
         t v
         h v
         s 0]
    (cond
      (= s n) (+ (apply max search) (apply min search)) ;;[(apply min search) (apply max search)]
      (> s n) (recur [] (rest h) (rest h) 0)
      (empty? v) 0
      :else (recur (conj search (first t)) (rest t) h (+ s (first t))))))

(find-cont sample-db 127)
(find-cont input 31161678)
