(ns tjtuom.aoc-2022.day-07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :refer [postwalk postwalk-demo]]))

(def INPUT (slurp (io/resource "inputs/day_07.txt")))

(defn str->int [s]
  (Integer/parseInt s))

(defn command? [line]
  (= \$ (first line)))

(def dir? map?)

(defn change-dir [state path]
  (case path
    "/" (assoc state :pwd ["/"])
    ".." (update state :pwd #(pop %))
    (update state :pwd #(conj % path))))

(defn parse-command [state line]
  (let [[_ cmd arg] (str/split line #" ")]
    (if (= "cd" cmd)
      (change-dir state arg)
      state)))

(defn parse-dir [state dir]
  (update-in state (:pwd state) assoc dir {}))

(defn parse-file [state size file]
  (update-in state (:pwd state) assoc file (Integer/parseInt size)))

(defn parse-file-or-dir [state line]
  (let [[l r] (str/split line #" ")]
    (if (= "dir" l)
      (parse-dir state r)
      (parse-file state l r))))

(defn parse-line [state line]
  (if (command? line)
    (parse-command state line)
    (parse-file-or-dir state line)))

(defn parse-file-tree [input]
  (->> input
       (str/split-lines)
       (reduce parse-line {:pwd ["/"] "/" {}})))

(defn files [dir]
  (->> dir
       (filter (comp (complement dir?) second))))

(defn dirs [dir]
  (filter (comp dir? second) dir))

(defn leaf? [dir]
  (not (seq (dirs dir))))

; (defn calculate-files-size [dir]
;   (->> dir
;        files
;        (map second)
;        (reduce +)))

; (defn calculate-dir-size [path dir]
;   (if (leaf? dir)
;     [path (calculate-files-size dir)]
;     (cons [path (calculate-files-size dir)] (mapcat #(list-dir-files-size (conj path (first %)) (second %)) (dirs dir)))))

; /foo/bar/baz 10 -> {[foo bar] 10 [foo] 10}

(defn path-map-for-file [path [file size]]
  (loop [m {}
         path path]
    (if-not (seq path)
      m
      (recur (update m path (fnil + 0) size) (pop path)))))

(dirs {"foo" {"bar" 10}})

(defn calculate-dir-sizes [path dir]
  (merge-with +
              (concat
               (->> (files dir)
                    (map (partial path-map-for-file path))
                    (merge-with +))
               (->> (dirs dir)
                    (map #(vector (conj path (first %) (second %))))
                    (map #(apply calculate-dir-sizes %))
                    (merge-with +)))))

(pprint (calculate-dir-sizes ["/"] (get (parse-file-tree INPUT) "/")))
