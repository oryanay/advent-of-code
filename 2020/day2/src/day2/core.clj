(ns day2.core
  (:require [clojure.string :as str]))

;; get input from file
(defn file->lines [file]
  (str/split-lines (slurp file)))

(defn str->int [s]
  (Integer/parseInt s))

(defn parse-line [l]
  (let [[n c s] (str/split l #" ")
        [mn mx] (str/split n #"-")
        [ch _] (str/split c #":")]
  [(str->int mn) (str->int mx) (first ch) s]))

;; (parse-line "1-3 a: abcde");; => ["1" "3" \a "abcde"]

(defn get-input []
  (map parse-line (file->lines "resources/input.txt")))

(def input-list (get-input))

;; -------------------------
;;
(defn is-valid? [[mn mx c s]]
  (<= mn (count (filter #(= c %) s)) mx))

;; (is-valid? [1 3 \c "abcde"]);; => true
;; (is-valid? [1 3 \b "cdefg"]);; => false

;;(count (filter is-valid? input-list));; => 625

(defn xor [p q]
  (and (or p q) (not (and p q))))
;; (xor true true)
;; (xor false false)
;; (xor true false)

(defn is-position-valid [[x y c s]]
  (let [p (= c (get s (- x 1)))
        q (= c (get s (- y 1)))]
    (xor p q)))

;; (is-position-valid [1 3 \c "abcde"]);; => true
;; (is-position-valid [1 3 \b "cdefg"]);; => false
;; (is-position-valid [2 9 \c "ccccccccc"]);; => false

(count (filter is-position-valid input-list))
