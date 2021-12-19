(ns aoc.day8
  (:require [aoc.core :as core]
            [clojure.string :as str]))

(def sample "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn parse-line [s]
  (re-matches #"^(\w{3}) ([\+\-]\d+)" s))
;; (re-matches #"^(\w{3}) ([\+\-])(\d+)" "acc +1")
;; (parse-line "jmp -1");; => ["jmp -1" "jmp" "-1"]

(def sample-db (->> sample
                    str/split-lines
                    (map parse-line)
                    (mapv (fn [[_ cmd line]] [cmd (Integer/parseInt line)]))))
sample-db;; => [["nop" 0]
;;     ["acc" 1]
;;     ["jmp" 4]
;;     ["acc" 3]
;;     ["jmp" -3]
;;     ["acc" -99]
;;     ["acc" 1]
;;     ["jmp" -4]
;;     ["acc" 6]]

(defn next-ind [cmd n]
  (if (= cmd "jmp") n 1))

(defn next-acc [cmd n acc]
  (if (= cmd "acc") (+ acc n) acc))

(defn compiler [xs]
  (let [max-ind (count xs)]
    (loop [ind 0
           history #{}
           last nil
           acc 0]
      (let [[cmd n] (xs ind)
            i (next-ind cmd n)]
        (if (or (contains? history ind) (>= ind max-ind)) last
              (recur (+ ind i) (conj history ind) acc (next-acc cmd n acc))
              )))))

(compiler sample-db)

;; let's try with the whole input
(def input
  (->> (core/read-file "resources/day8-input.txt" parse-line)
       (mapv (fn [[_ cmd line]] [cmd (Integer/parseInt line)]))))

input
(compiler input);; => 1832

;; second attempt with map and iterator
{:acc 0 :ind 0 :history #{}}

(defn not-in-history? [m]
  (not (contains? (:history m) (:ind m))))

(defn next-step [m]
  (let [[cmd n] (input (:ind m))
        i (+ (:ind m) (next-ind cmd n))]
    (assoc m
           :history (conj (:history m) (:ind m))
           :ind i
           :acc (next-acc cmd n (:acc m)))))

(:acc (last (take-while not-in-history? (iterate next-step {:acc 0 :ind 0 :history #{}}))));; => 1832
(count (:history (last (take-while not-in-history? (iterate next-step {:acc 0 :ind 0 :history #{}})))));; => 1832



;; part two
;;

;; compile once -> history
;; change one directive -> history
;; change next directive -> history
(def init-map {:acc 0 :ind 0 :history #{}})

(defn cmd-step [m [cmd n]]
  (let [i (+ (:ind m) (next-ind cmd n))]
    (assoc m
           :history (conj (:history m) (:ind m))
           :ind i
           :acc (next-acc cmd n (:acc m)))))

(defn in-history? [m]
  (contains? (:history m) (:ind m)))

(defn switch-cmd [cmd]
  (case cmd
  "jmp" "nop"
  "nop" "jmp"))

(defn flow-ends? [xs xm last-ind]
  (prn xm)
    (loop [m xm]
      (cond (> (:ind m) last-ind) (:acc m)
            (in-history? m) nil
            :else (recur (cmd-step m (get xs (:ind m)))))))
;; (flow-ends? (assoc sample-db  7 ["nop" -4]) init-map (count sample-db))

;; this one solves the puzzle but is inefficient and not ellegant
(defn search-bug [xs]
  (let [last-ind (count xs)]
    (loop [m {:acc 0 :ind 0 :history #{}}]
      (let [[cmd n] (xs (:ind m))]
        (cond
          (= "acc" cmd) (recur (cmd-step m [cmd n]))
          (>= (:ind m) last-ind) (:acc m)
          (flow-ends? (assoc xs (:ind m) [(switch-cmd cmd) n]) m last-ind) (flow-ends? (assoc xs (:ind m) [(switch-cmd cmd) n]) m last-ind)
          :else (recur (cmd-step m [cmd n])))))))

;; considering to create multiple mutations of the original program, and run each until one returns the right answer.
