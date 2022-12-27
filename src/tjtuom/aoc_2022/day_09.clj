(ns tjtuom.aoc-2022.day-09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [is are deftest] :as test]))

(def INPUT (slurp (io/resource "inputs/day_09.txt")))

(defn str->int [s]
  (Integer/parseInt s))

(def str->dir {"R" :right "L" :left "U" :up "D" :down})
(def delta {:left [-1 0] :right [1 0] :up [0 1] :down [0 -1]})

(defn parse-line [line]
  (let [[d n] (str/split line #" ")]
    [(str->dir d) (str->int n)]))

(defn expand-move [[d n]]
  (repeat n d))

(defn parse-lines [input]
  (->> input
       (str/split-lines)
       (map str/trim)
       (map parse-line)
       (mapcat expand-move)))

(defn apply-delta [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn tail-delta [[tx ty] [hx hy]]
  (let [dx (- hx tx)
        dy (- hy ty)]
    (case [dx dy]
      ; straight up/down/left/right
      [2 0] [1 0]
      [-2 0] [-1 0]
      [0 2] [0 1]
      [0 -2] [0 -1]
      ; diagonals
      [2 1] [1 1]
      [1 2] [1 1]
      [-2 1] [-1 1]
      [-1 2] [-1 1]
      [2 -1] [1 -1]
      [1 -2] [1 -1]
      [-2 -1] [-1 -1]
      [-1 -2] [-1 -1]
      [2 2] [1 1]
      [-2 -2] [-1 -1]
      ; default
      [0 0])))

(defn move-rope* [head tail]
  (lazy-seq
   (cons head
         (when (seq tail)
           (move-rope* (apply-delta (first tail) (tail-delta (first tail) head)) (rest tail))))))

(defn move-rope [[head & tail] move]
  (let [delta (delta move)
        new-head (apply-delta head delta)]
    (move-rope* new-head tail)))

(defn part-one [input]
  (->> input
       (parse-lines)
       (reductions move-rope [[0 0] [0 0]])
       (map second)
       (distinct)
       (count)))

(defn part-two [input]
  (->> input
       (parse-lines)
       (reductions move-rope (repeat 9 [0 0]))
       (map last)
       (distinct)
       (count)))

(defn print-rope [rope]
  (let [grid (vec (repeat 6 (vec (repeat 6 \.))))
        indexed (map-indexed #(vector %1 %2) rope)
        grid (reduce #(assoc-in %1 (vec (reverse (second %2))) (first %2)) grid indexed)]
    (println (str/join \newline (reverse (map str/join grid))))))

(let [rope (repeat 10 [0 0])
      moves (mapcat #(repeat (first %) (second %)) [[5 :right] [8 :up] [8 :left] [3 :down] [17 :right] [10 :down] [25 :left] [20 :up]])])

(part-two INPUT)

(deftest move-rope-test
  (are [current move new]
       (= new (move-rope current move))

    [[1 1] [0 0]] :right [[2 1] [1 1]]
    [[1 1] [0 0]] :up [[1 2] [1 1]]
    [[1 1] [0 0]] :left [[0 1] [0 0]]
    [[1 1] [0 0]] :down [[1 0] [0 0]]

    [[-1 1] [0 0]] :right [[0 1] [0 0]]
    [[-1 1] [0 0]] :up [[-1 2] [-1 1]]
    [[-1 1] [0 0]] :left [[-2 1] [-1 1]]
    [[-1 1] [0 0]] :down [[-1 0] [0 0]]

    [[-1 -1] [0 0]] :right [[0 -1] [0 0]]
    [[-1 -1] [0 0]] :up [[-1 0] [0 0]]
    [[-1 -1] [0 0]] :left [[-2 -1] [-1 -1]]
    [[-1 -1] [0 0]] :down [[-1 -2] [-1 -1]]

    [[1 -1] [0 0]] :right [[2 -1] [1 -1]]
    [[1 -1] [0 0]] :up [[1 0] [0 0]]
    [[1 -1] [0 0]] :left [[0 -1] [0 0]]
    [[1 -1] [0 0]] :down [[1 -2] [1 -1]]))

(def TEST_INPUT "R 4
                 U 4
                 L 3
                D 1
                R 4
                D 1
                L 5
                R 2")

(deftest part-one-test
  (is (= 13 (part-one TEST_INPUT))))

(test/run-tests)
