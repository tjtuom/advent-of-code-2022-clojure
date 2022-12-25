(ns tjtuom.aoc-2022.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [is are deftest] :as test]))

(def INPUT (slurp (io/resource "inputs/day_08.txt")))

(defn str->int [s]
  (Integer/parseInt s))

(defn parse-line [line]
  (->> (str/split line #"")
       (mapv str->int)))

(defn parse-grid [input]
  (->> input
       (str/split-lines)
       (mapv parse-line)))

(defn transpose [m]
  (apply mapv vector m))

(defn left [trees-rows [x y]]
  (vec (reverse (subvec (get trees-rows y) 0 x))))

(defn right [trees-rows [x y]]
  (subvec (get trees-rows y) (inc x)))

(defn top [trees-cols [x y]]
  (vec (reverse (subvec (get trees-cols x) 0 y))))

(defn bottom [trees-cols [x y]]
  (subvec (get trees-cols x) (inc y)))

(defn edge? [trees-rows trees-cols [x y]]
  (or (= 0 x)
      (= 0 y)
      (= y (dec (count trees-rows)))
      (= x (dec (count trees-cols)))))

(defn visible-horizontal? [trees-rows [x y]]
  (let [height (get-in trees-rows [y x])
        left (left trees-rows [x y])
        right (right trees-rows [x y])]
    (or
     (not (seq left))
     (not (seq right))
     (< (apply max left) height)
     (< (apply max right) height))))

(defn visible-vertical? [trees-cols [x y]]
  (let [height (get-in trees-cols [x y])
        top (top trees-cols [x y])
        bottom (bottom trees-cols [x y])]
    (or
     (not (seq top))
     (not (seq bottom))
     (< (apply max top) height)
     (< (apply max bottom) height))))

(defn visible? [trees-rows trees-cols [x y]]
  (or (edge? trees-rows trees-cols [x y])
      (visible-horizontal? trees-rows [x y])
      (visible-vertical? trees-cols [x y])))

(defn count-visible [trees-rows]
  (let [trees-cols (transpose trees-rows)]
    (count (filter identity (for [y (range (count trees-rows))
                                  x (range (count trees-cols))]
                              (visible? trees-rows trees-cols [x y]))))))

(defn viewing-distance [height neighbors]
  (min (count neighbors) (inc (count (take-while (partial > height) neighbors)))))

(defn scenic-score [trees-rows trees-cols [x y]]
  (let [h (get-in trees-rows [y x])]
    (* (viewing-distance h (top trees-cols [x y]))
       (viewing-distance h (bottom trees-cols [x y]))
       (viewing-distance h (left trees-rows [x y]))
       (viewing-distance h (right trees-rows [x y])))))

(defn part-one []
  (let [trees-rows (parse-grid INPUT)]
    (count-visible trees-rows)))

(defn part-two []
  (let [trees-rows (parse-grid INPUT)
        trees-cols (transpose trees-rows)]
    (apply max (for [y (range (count trees-rows))
                     x (range (count trees-cols))]
                 (scenic-score trees-rows trees-cols [x y])))))

(def grid-str "30373
25512
65332
33549
35390")

(def grid (parse-grid grid-str))

(deftest top-test
  (are [point trees]
       (= trees (top (transpose grid) point))
    [2 2] [5 3]
    [0 1] [3]
    [3 4] [4 3 1 7]))

(deftest bottom-test
  (are [point trees]
       (= trees (bottom (transpose grid) point))
    [2 2] [5 3]
    [0 1] [6 3 3]
    [3 4] []))

(deftest left-test
  (are [point trees]
       (= trees (left grid point))
    [2 2] [5 6]
    [0 1] []
    [3 4] [3 5 3]))

(deftest right-test
  (are [point trees]
       (= trees (right grid point))
    [2 2] [3 2]
    [0 1] [5 5 1 2]
    [3 4] [0]))

(deftest viewing-distance-test
  (are [h neighbors distance]
       (= distance (viewing-distance h neighbors))
    3 [2 2 3] 3
    1 [1 2 3] 1
    4 [2 3 3 5] 4

    5 [3 5 3] 2
    5 [3 3] 2
    5 [3] 1
    5 [4 9] 2))

(deftest scenic-score-test
  (are [point score]
       (= score (scenic-score grid (transpose grid) point))
    [2 1] 4
    [2 3] 8))

(deftest edge-test
  (are [point is-edge?]
       (= is-edge? (edge? grid (transpose grid) point))
    [0 0] true
    [0 1] true
    [1 0] true
    [4 0] true
    [0 4] true
    [1 1] false
    [2 2] false
    [3 3] false))

(deftest visible-horizontal-test
  (are [point is-visible?]
       (= is-visible? (visible-horizontal? grid point))
    [1 1] true
    [2 1] true
    [3 1] false

    [1 2] true
    [2 2] false
    [3 2] true

    [1 3] false
    [2 3] true
    [3 3] false))

(deftest visible-vertical-test
  (are [point is-visible?]
       (= is-visible? (visible-vertical? (transpose grid) point))
    [1 1] true
    [2 1] true
    [3 1] false

    [1 2] false
    [2 2] false
    [3 2] false

    [1 3] false
    [2 3] true
    [3 3] false))

(deftest visible-test
  (are [point is-visible?]
       (= is-visible? (visible? grid (transpose grid) point))
    [0 0] true
    [0 1] true
    [1 0] true
    [4 0] true
    [0 4] true

    [1 1] true
    [2 1] true
    [3 1] false

    [1 2] true
    [2 2] false
    [3 2] true

    [1 3] false
    [2 3] true
    [3 3] false))

(deftest count-visible-test
  (is (= 21 (count-visible grid))))

(test/run-tests)

