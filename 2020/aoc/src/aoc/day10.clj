(ns aoc.day10)

(def sample1 [0 28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(sort sample1);; => (1 2 3 4 7 8 9 10 11 14 17 18 19 20 23 24 25 28 31 32 33 34 35 38 39 42 45 46 47 48 49)
(def small [16 10 15 5 1 11 7 19 6 12 4])
(sort small);; => (1 4 5 6 7 10 11 12 15 16 19)

;;                 1
;;                 4 5 6 7
;;                 10 11 12
;;                 15
;;                 16
;;                 19

(->> sample1
     sort
     (partition 2 1)
     (map (fn [[a b]] (- b a)))
     frequencies)

(def input [0 80 87 10 122 57 142 134 59 113 139
            101 41 138 112 46 96 43 125 36 54
            133 17 42 98 7 114 78 67 77 28 149
            58 20 105 31 19 18 27 40 71 117 66
            21 72 146 90 97 94 123 1 119 30 84
            61 91 118 2 29 104 73 13 76 24 148
            68 111 131 83 49 8 132 9 64 79 124
            95 88 135 3 51 39 6 60 108 14 35
            147 89 34 65 50 145 128])

(->> input
     sort
     (partition 2 1)
     (map (fn [[a b]] (- b a)))
     frequencies);; => {1 64, 3 28}
;; add one to each result (from 0 to device +3 from max)
;; result is:
(* 65 29);; => 1885

;; part 2
;;
;; The number of option when there is a 1 1 diff is 1
;; between 2 ones 2
;; between 3 ones 4
;; between 4 ones 7
;; So the number of combinations depends on how many straight one diffs there are, multiplied by each combination

(defn factorial [n]
  (reduce *' (range 1 (inc n))))


(->> input
     sort
     (partition 2 1)
     (map (fn [[a b]] (- b a)))
     (partition-by (partial = 3))
     (remove (fn [c] (some (partial = 3) c)))
     (map count)
     (map {1 1 2 2 3 4 4 7})
     (apply *));; => 2024782584832
