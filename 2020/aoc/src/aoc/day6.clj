(ns aoc.day6
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(def input (core/read-file "resources/day6-input.txt" #(str/split % #"\n\n") identity))
(count (distinct (filter #(not= % \newline) (first input))))

(defn remove-newline [s] (filter #(not= % \newline) s))
(->> input
     (map remove-newline)
     (map distinct)
     (map count)
     (apply +));; => 6506


(def example (str/split "abc

a
b
c

ab
ac

a
a
a
a

b" #"\n\n"))

(defn filter-unique [s]
  (let [rec (str/split s #"\n")]
    (count (filter (fn [[_ v]] (= v (count rec))) (frequencies (mapcat seq rec))))))

(apply + (map filter-unique input))

(let [r (str/split (nth example 2) #"\n")]
  (filter (fn [[_ v]] (= v (count r))) (frequencies (mapcat seq r))))

  ;; (count r) (frequencies (mapcat seq r))])


(frequencies (seq "abc"))
