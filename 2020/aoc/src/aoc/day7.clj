(ns aoc.day7
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.set :as set]
            [aoc.core :as core]))

(def example "light red bags contain 1 bright white bag, 2 muted yellow bags.")

;; ---------- parse the string ---------------
(defn parse-sub-rule
  "first attempt at parsing with split"
  [s]
  (let [[_ n bag] (re-matches #" ?(\d)? ?(\w+ \w+).+" s)]
    [bag (Integer/parseInt (or n "0"))]))
;; (parse-sub-rule " 2 muted yellow bags.");; => ["muted yellow" 2]

(defn parse-rule
  "parse a rule string with the format of bag-type contains n1 bag-type, n2 bagtype...
  the function parameter will decide what type of output to produce after string parsing"
  [f s]
  (let [[hyp rules] (str/split s #" bags contain ")
        bags (map parse-sub-rule (str/split rules #","))]
    (f hyp bags)))

(defn reverse-lookup [hyp bags]
  (for [x bags]
      [(first x) hyp]))
;; (parse-rule reverse-lookup example);; => (["bright white" "light red"] ["muted yellow" "light red"])

(defn bag-lookup [hyp bags]
  {hyp bags})
;; (parse-rule bag-lookup example);; => {"light red" (["bright white" 1] ["muted yellow" 2])}
;; (parse-rule reverse-lookup "dotted black bags contain no other bags.");; => (["no other" "dotted black"])
;; (parse-sub-rule "no other bags.");; => ["no other" 0]
;; (parse-sub-rule "2 muted yellow bags.");; => ["muted yellow" 2]

;;Try to parse with a single regex
(defn parse-line [s]
  (let [[[_ _ bag] & r] (re-seq #"(?:^|(\d) )(\w+ \w+) bags?" s)]
    {bag (map (fn [[_ n b]] [(Integer/parseInt n) b]) r)}))
;; (re-seq #"(?:^|(\d) )(\w+ \w+) bags?"  example);; => (["light red bags" nil "light red"] ["1 bright white bag" "1" "bright white"] ["2 muted yellow bags" "2" "muted yellow"])
;; (parse-line example);; => {"light red" ([1 "bright white"] [2 "muted yellow"])}

(def sample "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(def sample2 "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(def sample-db (apply merge (map parse-line (str/split-lines sample))))
(def sample2-db (apply merge (map parse-line (str/split-lines sample2))))
;; sample-db;; => {"muted yellow" ([2 "shiny gold"] [9 "faded blue"]),
;;     "light red" ([1 "bright white"] [2 "muted yellow"]),
;;     "dotted black" (),
;;     "dark orange" ([3 "bright white"] [4 "muted yellow"]),
;;     "bright white" ([1 "shiny gold"]),
;;     "shiny gold" ([1 "dark olive"] [2 "vibrant plum"]),
;;     "faded blue" (),
;;     "vibrant plum" ([5 "faded blue"] [6 "dotted black"]),
;;     "dark olive" ([3 "faded blue"] [4 "dotted black"])}
(def input (apply merge (core/read-file "resources/day7-input.txt" parse-line))) ;; input from file

;; (bag-exists? "shiny gold"  [2 "shiny gold"])

;; my second attempt solution inspired by fred overflow youtube
;; scan the entire map recursively.
(defn search-container
  "recursively search the graph to find if a containing bag can hold the bag we want."
  [container with-color]
  (->> container
       sample2-db
       (some (fn [[_ color]]
               (or (= color with-color)
                   (search-container color with-color))))))
;; (search-container "light red" "shiny gold");; => true

(->> sample2-db
     (filter (fn [[k _]] (search-container k "shiny gold")))
     count)

;; second solution draft - traverse the bags graph with accumulation
(defn walk-bags [container]
  (->> container
       input
       (reduce (fn [v [n color]] (+ v n (* n (walk-bags color)))) 0)))

(walk-bags "shiny gold");; => 12128

(defn add
  "Adds key-value pairs to a map value.
  The purpose is a simple implementation of a multimap where a single key can hold multiple values."
  ([m [k v]]
   (add m k v))
  ([m k v]
   (assoc m k (conj (get m k #{}) v)))
  ([m k v & kvs]
   (apply add (add m k v) kvs)))


(def sample-map (reduce add {} (mapcat identity (map (partial parse-rule reverse-lookup) (str/split-lines sample))))) ;; a sample for "reverse lookup" where you want to know which bags can contain a shiny gold bag
(def sample-bags (apply merge (map (partial parse-rule bag-lookup) (str/split-lines sample)))) ;; a sample for forward search where you want to find how many bags each bag can hold

sample-map
;; => {"bright white" #{"light red" "dark orange"},
;;     "muted yellow" #{"light red" "dark orange"},
;;     "shiny gold" #{"muted yellow" "bright white"},
;;     "faded blue" #{"muted yellow" "vibrant plum" "dark olive"},
;;     "dark olive" #{"shiny gold"},
;;     "vibrant plum" #{"shiny gold"},
;;     "dotted black" #{"vibrant plum" "dark olive"},
;;     "no other" #{"dotted black" "faded blue"}}

sample-bags
;; => {"muted yellow" (["shiny gold" 2] ["faded blue" 9]),
;;     "light red" (["bright white" 1] ["muted yellow" 2]),
;;     "dotted black" (["no other" 0]),
;;     "dark orange" (["bright white" 3] ["muted yellow" 4]),
;;     "bright white" (["shiny gold" 1]),
;;     "shiny gold" (["dark olive" 1] ["vibrant plum" 2]),
;;     "faded blue" (["no other" 0]),
;;     "vibrant plum" (["faded blue" 5] ["dotted black" 6]),
;;     "dark olive" (["faded blue" 3] ["dotted black" 4])}

(defn traverse [m name]
  (loop [r #{}
         k (get m name)]
    (let [val-map (select-keys m k)]
      (if (empty? val-map) (set/union r k)
          (recur (set/union r k) (apply set/union (vals val-map)))))))

(def input-map (reduce add {} (mapcat identity input))) ;; parsed input to multi-map
(traverse sample-map "shiny gold")
(count (traverse input-map "shiny gold")) ;; ----------- 1st answer result
