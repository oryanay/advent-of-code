(ns aoc.day2 (:require [clojure.string :as str]
                       [aoc.core :as core]))

(def sample "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn parse-line [s]
  (let [p (str/split s #" ")]
    (zipmap [:direction :size] [(first p) (Integer/parseInt (second p))])))
(parse-line "forward 5")

(->> sample str/split-lines (map parse-line))

;; part 1
(defn step
  [pos cmd]
  (case (:direction cmd)
    "down" (update pos :y + (:size cmd))
    "up" (update pos :y - (:size cmd))
    "forward" (update pos :x + (:size cmd))
    :else pos))

(step {:x 0 :y 1} {:direction "down" :size 4})
(step {:x 0 :y 5} {:direction "up" :size 4})
(step {:x 0 :y 5} {:direction "forward" :size 4})

(->> sample str/split-lines (map parse-line)
     (reduce (fn [v c] (step v c)) {:x 0 :y 0}))

(def input (core/read-file "resources/day2.txt" parse-line))

(->> (reduce (fn [v c] (step v c)) {:x 0 :y 0} input)
     vals
     (apply *));; => 1451208

;; part 2
(defn new-step
  [pos cmd]
  (case (:direction cmd)
    "down" (update pos :aim + (:size cmd))
    "up" (update pos :aim - (:size cmd))
    "forward" (->  pos
                   (update :x + (:size cmd))
                   (update :y (fn [a b] (+ a (* b (:aim pos)))) (:size cmd)))))
;; (* (:aim cmd) %)
;; (new-step {:x 0 :y 0 :aim 4} {:direction "forward" :size 5})

(->> sample str/split-lines (map parse-line)
     (reduce (fn [v c] (new-step v c)) {:x 0 :y 0 :aim 0}))

(->> input
     (reduce (fn [v c] (new-step v c)) {:x 0 :y 0 :aim 0})
     (#(* (:x %) (:y %))));; => 1620141160
