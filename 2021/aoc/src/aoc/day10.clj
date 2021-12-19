(ns aoc.day10
  (:require [clojure.string :as str]
            [aoc.core :as core]))

(def sample "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(->> sample
     str/split-lines
     (mapv seq)
     first);; => (\[ \( \{ \( \< \( \( \) \) \[ \] \> \[ \[ \{ \[ \] \{ \< \( \) \< \> \>)


(defn push? [c] (some #(= c %) [\[ \( \{ \< ]))
(defn pop? [c] (some #(= c %)  [\] \) \} \>]))
;; (push? \[);; => true
;; (pop? \});; => true

(def pairs {\[ \] \( \) \{ \} \< \>})
(defn match? [a b] (= (get pairs a) b))
;; (match? \[ \]);; => \]

(defn push-or-pop [v q]
  (cond
    (push? v) (conj q v)
    (and (pop? v) (match? (peek q) v)) (pop q)
    :else (conj q [\C v])
    ))

(seq "<{([([[(<>()){}]>(<<{{");; => (\< \{ \( \[ \( \[ \[ \( \< \> \( \) \) \{ \} \] \> \( \< \< \{ \{)

(defn reduce-chunks [s] (reduce (fn [q c] (push-or-pop c q)) [] (seq s)))
(defn corrupted-char [s]
  (some #(when (coll? %) (second %) ) (reduce-chunks s)))

(def score-board {\) 3 \] 57 \} 1197 \> 25137} )

(->> sample
     str/split-lines
     (mapv seq)
     (keep corrupted-char)
     (map #(get score-board %))
     (reduce +))

(def input (core/read-file "resources/day10.txt" seq))

(->> input
     (keep corrupted-char)
     (map #(get score-board %))
     (reduce +));; => 268845

;; part 2 auto-complete

(defn corrupt? [s]
  (every? char? s))

(defn auto-complete [s]
  (reduce (fn [q c] (conj q (get pairs c))) '() s))


(def score-board2 {\) 1 \] 2 \} 3 \> 4} )

(defn calculate-score [s]
  (reduce (fn [v c] (+ (* v 5) (get score-board2 c))) 0 s))

(defn median [v]
  (nth v (quot (count v) 2)))

(->> sample
     str/split-lines
     (mapv seq)
     (map reduce-chunks)
     (filter corrupt?)
     (map auto-complete)
     (map calculate-score)
     sort
     vec
     median
     );; => 288957

(->> input
     (map reduce-chunks)
     (filter corrupt?)
     (map auto-complete)
     (map calculate-score)
     sort
     vec
     median
     );; => 4038824534
