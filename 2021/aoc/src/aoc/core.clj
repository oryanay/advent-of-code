(ns aoc.core)

(ns aoc.core
  (:require [clojure.string :as str]))

;; get input from file
(defn file->lines [file]
  (str/split-lines (slurp file)))

(defn str->int [s]
  (Integer/parseInt s))

(defn get-input [parser file]
  (map parser (file->lines file)))

(defn read-file
  ([file parser]
   (read-file file str/split-lines parser))
  ([file splitter parser]
   (->> file
        slurp
        splitter
        (map parser))))
