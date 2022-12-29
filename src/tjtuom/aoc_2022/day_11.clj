(ns tjtuom.aoc-2022.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [is are deftest] :as test]))

(def INPUT (slurp (io/resource "inputs/day_10.txt")))

(defn str->int [s]
  (Integer/parseInt s))

(defn parse-items [s]
  (mapv str->int (re-seq #"\d+" s)))

(def str->op {"+" + "-" - "*" * "/" /})

(defn parse-op [s]
  (let [[_ expr] (str/split s #" = ")
        [lhs op rhs] (str/split expr #" ")]
    (fn [v]
      ((str->op op)
       (if (= "old" lhs) v (str->int lhs))
       (if (= "old" rhs) v (str->int rhs))))))

(defn parse-test [s]
  (let [n (str->int (re-find #"\d+" s))]
    (fn [v]
      (= 0 (rem v n)))))

(defn parse-target [s]
  (str->int (re-find #"\d+" s)))

(defn parse-monkey [s]
  (let [[_ item-str op-str test-str true-str false-str] (str/split-lines s)]
    {:items (parse-items item-str)
     :op (parse-op op-str)
     :test (parse-test test-str)
     :true-target (parse-target true-str)
     :false-target (parse-target false-str)}))

(defn parse-input [input]
  (->> input
       (str/split input "\n\n")
       (mapv parse-monkey)))

(assoc-in [{:name "foo"}] [0 :name] "bar")
