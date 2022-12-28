(ns tjtuom.aoc-2022.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [is are deftest] :as test]))

(def INPUT (slurp (io/resource "inputs/day_10.txt")))

(defn str->int [s]
  (Integer/parseInt s))

(defn parse-line [line]
  (if (= line "noop")
    [:noop]
    [:addx (str->int (second (str/split line #" ")))]))

(defn preprocess [[cmd v]]
  (case cmd
    :noop [[:noop]]
    :addx [[:noop] [:addx v]]
    [cmd v]))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map str/trim)
       (map parse-line)
       (mapcat preprocess)))

(defn run [register [cmd v]]
  (case cmd
    :noop register
    :addx (+ register v)
    register))

(defn cycles [instructions]
  (reductions run 1 instructions))

(defn signal-strength [[i register]]
  (* i register))

(defn part-one [input]
  (->> input
       parse-input
       cycles
       (map-indexed #(vector (inc %1) %2))
       (filter #(#{20 60 100 140 180 220} (first %)))
       (map signal-strength)
       (reduce +)))

(def TEST_INPUT (slurp (io/resource "inputs/day_10_test.txt")))

(deftest part-one-test
  (is (= 13140 (part-one TEST_INPUT))))

(test/run-tests)
