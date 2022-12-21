(ns tjtuom.aoc-2022.day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def INPUT (slurp (io/resource "inputs/day_05.txt")))

(defn str->int [s]
  (Integer/parseInt s))

(defn transpose [m]
  (apply mapv vector m))

(defn parse-line [s]
  (->> s
       (partition 3 4)
       (mapv second)))

(defn parse-crates [s]
  (->> s
       (str/split-lines)
       butlast
       (mapv parse-line)
       transpose
       (mapv #(vec (reverse %)))
       (mapv #(vec (remove #{\space} %)))))

(defn crates->str [m]
  (->> m
       (mapv #(vec (reverse %)))
       transpose))

(defn parse-move [s]
  (mapv str->int (re-seq #"\d+" s)))

(defn parse-moves [s]
  (->> s
       (str/split-lines)
       (mapv parse-move)))

(defn peek [state n]
  (clojure.core/peek (state n)))

(defn pop [state n]
  (update state n clojure.core/pop))

(defn put [state n c]
  (update state n #(conj % c)))

(defn move [state [n from to]]
  (if (zero? n)
    state
    (let [c (peek state from)]
      (recur (put (pop state from) to c) (dec n) from to))))

(def crates (first (str/split INPUT #"\n\n")))
(def moves (second (str/split INPUT #"\n\n")))

(defn part-one []
  (let [[crates moves] (str/split INPUT #"\n\n")
        crates (parse-crates crates)
        moves (parse-moves moves)]
    (reduce)))
