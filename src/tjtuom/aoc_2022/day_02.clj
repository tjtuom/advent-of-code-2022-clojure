(ns tjtuom.aoc-2022.day-02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def INPUT (slurp (io/resource "inputs/day_02.txt")))

(defn str->int [s]
  (Integer/parseInt s))

(def opp->move {"A" :rock "B" :paper "C" :scissors})
(def me->move {"X" :rock "Y" :paper "Z" :scissors})

(defn parse-move [s]
  (let [[opp me] (str/split s #" ")]
    [(me->move me) (opp->move opp)]))

(defn result [moves]
  (case moves
    [:rock :paper] 0
    [:rock :rock] 3
    [:rock :scissors] 6
    [:paper :scissors] 0
    [:paper :paper] 3
    [:paper :rock] 6
    [:scissors :rock] 0
    [:scissors :scissors] 3
    [:scissors :paper]))

(defn value [[me _]]
  ({:rock 1 :paper 2 :scissors 3} me))

(defn score [moves]
  (+ (result moves) (value moves)))

(score (parse-move "C Z"))

(defn part-one []
  (->> INPUT
       (str/split-lines)
       (map parse-move)
       (map score)
       (reduce +)))

(def code->result {"X" :loss "Y" :draw "Z" :win})
(def opp-result->move
  {[:rock :win] :paper
   [:rock :draw] :rock
   [:rock :loss] :scissors
   [:paper :win] :scissors
   [:paper :draw] :paper
   [:paper :loss] :rock
   [:scissors :win] :rock
   [:scissors :draw] :scissors
   [:scissors :loss] :paper})

(defn parse-move-2 [s]
  (let [[opp res] (str/split s #" ")
        opp (opp->move opp)
        me (opp-result->move [opp (code->result res)])]
    [me opp]))

(defn part-two []
  (->> INPUT
       (str/split-lines)
       (map parse-move-2)
       (map score)
       (reduce +)))

