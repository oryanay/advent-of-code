(ns aoc.day12
  (:require [clojure.string :as str]))

(def sample "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc ")

(def small-sample "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")

(def large-sample "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW")

(def input "start-qs
qs-jz
start-lm
qb-QV
QV-dr
QV-end
ni-qb
VH-jz
qs-lm
qb-end
dr-fu
jz-lm
start-VH
QV-jz
VH-qs
lm-dr
dr-ni
ni-jz
lm-QV
jz-dr
ni-end
VH-dr
VH-ni
qb-HE")

(defn map-add [[k v] m]
  (if (= v "start") m
      (if-let [x (get m k)]
        (assoc m k (conj x v))
        (assoc m k [v]))))
;; (map-add ["a" "b"] {});; => {"a" ["b"]}

(defn coll->map [coll]
  (reduce (fn [v c] (->> v
                        (map-add c)
                        (map-add (reverse c))
                        ))
          {} coll))
;; (coll->map '(["a" "b"]));; => {"a" ["b"], "b" ["a"]}
;; (coll->map '(["a" "start"]));; => {"start" ["a"]}

(defn lowercase? [s] (every? #(Character/isLowerCase %) s))

;; => {"dc" ["end" "start" "HN" "LN" "kj"],
;;     "end" ["dc" "HN"],
;;     "HN" ["start" "dc" "end" "kj"],
;;     "start" ["HN" "kj" "dc"],
;;     "kj" ["start" "sa" "HN" "dc"],
;;     "LN" ["dc"],
;;     "sa" ["kj"]}
;;
;;           sa
;;           |
;;        -- kj ----
;;      |    |      |
;;      |    |  end |
;;      |    | /   \|
;;  start -- HN -- dc -- LN
;;      |           |
;;       ----------
;;
(defn visit [coll node]
  (if (lowercase? node) (conj coll node) coll))

(defn visited? [visited node]
  (contains? (set visited) node))

(defn find-routes [graph node visited route]
  (if (= node "end") 1
      (map
       (fn [n]
         (if (visited? visited n)
           0
           (find-routes graph n (visit visited node) (conj route n))))
       (get graph node))))


(prn "---------")
(->> input
     str/split-lines
     (map str/trim)
     (map #(str/split % #"-"))
     coll->map
     ;; (#(find-routes-map % "start" #{} ["start"]))
     (#(find-routes % "start" #{} ["start"]))
     flatten
     (reduce +)
     );; => 5178

;; part 2 - visit one cave twice

(defn visited-freq? [xm node]
    (and (contains? xm node) (some #(<= 2 %) (vals xm))))
;; (visited-freq? {"b" 1 "d" 1} "b")
;; (visited-freq? {"b" 2 "d" 1} "d")
;; (visited-freq? {"start" 1, "kj" 2, "dc" 1} "dc")
(visited-freq? {"start" 1, "dc" 2, "kj" 1} "dc")

(defn visit-freq [xm node]
  (cond
    (not (lowercase? node)) xm
    (contains? xm node) (update xm node inc)
    :else (assoc xm node 1)))

;; (visit-freq {"b" 1} "a")
;; (visit-freq {"b" 1 "a" 2} "a")


(defn find-freq-routes [graph node visited route]
  (if (= node "end") 1
      (map
       (fn [n]
         (cond
           (visited-freq? visited n) 0
           (visited-freq? visited node) 0
           :else (find-freq-routes graph n (visit-freq visited node) (conj route n))))
       (get graph node))))

(->> input
     str/split-lines
     (map str/trim)
     (map #(str/split % #"-"))
     coll->map
     (#(find-freq-routes % "start" {} ["start"]))
     flatten
     (reduce +)
     );; => 130094
