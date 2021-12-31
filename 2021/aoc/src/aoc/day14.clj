(ns aoc.day14
  (:require [clojure.string :as str]
            [aoc.core :as core]))

(def sample "CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(defn parse-cmd [s]
  (let [[a b] (str/split s #" -> ")]
    [(seq a) (first (seq b))]))
;; (parse-cmd "CH -> B") ;; => [(\C \H) \B]

(defn find-polymer [p m]
  (if-let [x (get m p)]
    [(first p) x (second p)]
    p))

(def sample-cmd (->> sample
                     str/split-lines
                     (map parse-cmd)
                     (into {})))
;; (find-polymer '(\N \N) sample-cmd) ;; => [\N \C \N]

(defn conjduce
  "conjduce is a made up name :) just a single iteration of inserting the letter from the lookup list to the string."
  [cmd s]
  (let [part (partition 2 1 s)
        coll (map #(find-polymer % cmd) part)]
    (->>
     (reduce
      (fn [v c]
        (conj v (rest c))) [(ffirst coll)] coll)
     flatten
     (apply str))))
;; (conjduce "NNCB") ;; => "NCNBCHB"

;; find the sample result
(->> (nth (iterate (partial conjduce sample-cmd) "NNCB") 10)
     frequencies
     (sort-by val)
     (#(- (val (last %)) (val (first %)))))
;; => 1588

;; find the part 1 result
(def input-cmd (into {} (core/read-file "resources/day14.txt" parse-cmd)))
(->> (nth (iterate (partial conjduce input-cmd) "PKHOVVOSCNVHHCVVCBOH") 10)
     frequencies
     (sort-by val)
     (#(- (val (last %)) (val (first %)))));; => 3048

;;
;; part 2 - handle really big numbers
;;
(defn cmd->pairs
  "Return a vector of lookup results, including the seperating letter for a given pair."
  [cmd]
  (into {} (map (fn [[k v]] (vector k [[(first k) v] [v (second k)] v])) cmd)))

(def sample-lookup (cmd->pairs sample-cmd))
(def input-lookup (cmd->pairs input-cmd))

(defn add
  "Return a counter map with updated values given a counter map, a key and its numeric value."
  [m v i]
  (if-let [j (get m v)]
    (assoc m v (+ i j))
    (assoc m v i)))
;; (add {'(\N \N) 1} '(\N \H)) => {(\N \N) 1, (\N \H) 1}
;; (add {'(\N \N) 1} '(\N \N) 2) ;; => {(\N \N) 3}

(defn map-add
  ([m coll] (map-add m coll 1))
  ([m coll i]
   (reduce
    (fn [v c] (add v c i))
    m coll)))

;; (defn mult [m v i]
;;   (if-let [j (get m v)]
;;     (assoc m v (* i j))
;;     (assoc m v 1)))

;; (defn map-mult
;;   ([m coll] (map-mult m coll 2))
;;   ([m coll i]
;;    (reduce
;;     (fn [v c] (mult v c i))
;;     m coll)))

;; create sample initial map counts
(def sample-m (->> "NNCB"
                   (partition 2 1)
                   (map-add {})
                   (merge (frequencies  "NNCB"))))

(defn single-char-counters [m]
  (into {} (filter (fn [[k _]] (char? k)) m)))

(defn counter [lookup polymer-map]
  (reduce
   (fn [m [k v]]
     (map-add m (get lookup k) v))
   (single-char-counters polymer-map)
   polymer-map))

;; (defn count-freq [m]
;;   (reduce (fn [m [k i]]
;;             (-> m
;;                 (update (first k) (fnil + 0) i )
;;                 (update (second k) (fnil + 0) i)))
;;           {} m))

;;(nth (iterate counter sample-m) 10)

(->> (nth (iterate (partial counter sample-lookup) sample-m) 40)
     single-char-counters
     (sort-by val)
     (#(- (val (last %)) (val (first %)))))

(def input-m (->>  "PKHOVVOSCNVHHCVVCBOH"
                   (partition 2 1)
                   (map-add {})
                   (merge (frequencies "PKHOVVOSCNVHHCVVCBOH"))))

(->> (nth (iterate (partial counter input-lookup) input-m) 40)
     single-char-counters
     (sort-by val)
     (#(- (val (last %)) (val (first %)))));; => 3288891573057
