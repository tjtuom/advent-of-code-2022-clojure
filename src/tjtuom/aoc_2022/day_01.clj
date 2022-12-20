(ns tjtuom.aoc-2022.day-01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def INPUT (slurp (io/resource "inputs/day_01.txt")))

(defn str->int [s]
  (Integer/parseInt s))

(defn sum-elf [lines]
  (->> (str/split-lines lines)
       (map str/trim)
       (map str->int)
       (reduce +)))

(defn part-one []
  (->> (str/split INPUT #"\n\n")
       (map sum-elf)
       (apply max)))

(part-one)

(defn part-two []
  (->> (str/split INPUT #"\n\n")
       (map sum-elf)
       (sort >)
       (take 3)
       (reduce +)))

(part-two)

