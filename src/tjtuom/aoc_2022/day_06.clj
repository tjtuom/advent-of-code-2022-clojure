(ns tjtuom.aoc-2022.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def INPUT (slurp (io/resource "inputs/day_06.txt")))

(defn str->int [s]
  (Integer/parseInt s))

(defn part-one [input]
  (->> input
       (partition 4 1)
       (map set)
       (take-while #(< (count %) 4))
       count
       (+ 4)))

(defn part-one [input]
  (->> input
       (partition 4 1)
       (map set)
       (take-while #(< (count %) 4))
       count
       (+ 4)))
