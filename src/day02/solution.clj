(ns day02.solution 
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; solution part 1

(defn- safe? [coll]
  (let [pairs (partition 2 1 coll)]
    (or (every? (fn [[a b]] (<= 1 (- a b) 3)) pairs)
        (every? (fn [[a b]] (<= 1 (- b a) 3)) pairs))))

(time
 (with-open [rdr (io/reader "src/day02/input.txt")]
   (->> rdr
        line-seq
        (map #(map Integer/new (str/split % #" ")))
        (reduce #(if (safe? %2) (inc %1) %1) 0))))

;; solution part 2

(defn- lazy-remove-one [coll]
  (map-indexed (fn [i _]
                 (concat (take i coll) (drop (inc i) coll)))
               coll))

(defn- safe?* [coll]
  (or (safe? coll)
      (some safe? (lazy-remove-one coll))))

(time
 (with-open [rdr (io/reader "src/day02/input.txt")]
   (->> rdr
        line-seq
        (map #(map Integer/new (str/split % #" ")))
        (reduce #(if (safe?* %2) (inc %1) %1) 0))))

