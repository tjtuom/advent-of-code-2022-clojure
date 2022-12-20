(ns tjtuom.aoc-2022.day-03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def INPUT (slurp (io/resource "inputs/day_03.txt")))

(defn compartments [s]
  (let [midpoint (/ (count s) 2)]
    [(take midpoint s) (drop midpoint s)]))

(defn in-both [[l r]]
  (first (set/intersection (set l) (set r))))

(defn priority [c]
  (if (Character/isUpperCase c)
    (- (int c) 38)
    (- (int c) 96)))

(def s "WjmsdnddnmQPZPPJPL")

(defn part-one []
  (->> INPUT
       (str/split-lines)
       (map compartments)
       (map in-both)
       (map priority)
       (reduce +)))

(defn find-badge [elves]
  (first (apply set/intersection (map set elves))))

(defn part-two []
  (->> INPUT
       (str/split-lines)
       (partition 3)
       (map find-badge)
       (map priority)
       (reduce +)))

(part-two)
