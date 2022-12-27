(ns tjtuom.aoc-2022.day-09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [is are deftest] :as test]))

(def INPUT (slurp (io/resource "inputs/day_08.txt")))

(defn str->int [s]
  (Integer/parseInt s))
