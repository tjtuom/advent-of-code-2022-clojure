(ns tjtuom.aoc-2022.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def INPUT (slurp (io/resource "inputs/day_04.txt")))

(defn str->int [s]
  (Integer/parseInt s))

(defn parse-pairs [s]
  (let [[l r] (str/split s #",")
        [a b] (str/split l #"-")
        [c d] (str/split r #"-")]
    (mapv str->int [a b c d])))

(defn contained? [[a b c d]]
  (or (<= a c d b) (<= c a b d)))

(defn part-one []
  (->> INPUT
       (str/split-lines)
       (map parse-pairs)
       (filter contained?)
       (count)))

(defn overlaps? [[a b c d]]
  (or (<= a c b) (<= a d b) (<= c a d) (<= c b d)))

(defn part-two []
  (->> INPUT
       (str/split-lines)
       (map parse-pairs)
       (filter overlaps?)
       (count)))

