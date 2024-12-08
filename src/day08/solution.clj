(ns day08.solution
  (:require
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))

(defn in-range?-fn [width height]
  (fn [[x y]]
    (and (< -1 x width)
         (< -1 y height))))

;; solution part 1

(defn- calculate-antipodes [[x1 y1] [x2 y2]]
  [[(- (* 2 x1) x2) (- (* 2 y1) y2)]
   [(- (* 2 x2) x1) (- (* 2 y2) y1)]])

(time
 (let [input (slurp "src/day08/input.txt")
       width (.indexOf input "\n")
       height (dec (/ (count input) width))
       in-range? (in-range?-fn width height)
       antennae
       (->> (str/replace input #"\n" "")
            (map-indexed vector)
            (reduce
             (fn [m [idx character]]
               (if (= \. character)
                 m
                 (let [[x y] [(mod idx height) (quot idx width)]]
                   (update m character conj [x y]))))
             {}))
       antinodes
       (->> antennae
            (reduce
            (fn [acc [_ points]]
              (let [pairs (combo/combinations points 2)]
                (reduce
                 (fn [coll [p1 p2]]
                   (let [[q1 q2] (calculate-antipodes p1 p2)]                     
                     (cond-> coll
                       (in-range? q1) (conj q1)
                       (in-range? q2) (conj q2))))
                 acc
                 pairs))) 
             #{}))]
   (count antinodes)))

;; solution part 2

(defn- calculate-antipodes* [in-range? [x1 y1] [x2 y2]]
  (into #{}
   (concat
    (for [n (iterate inc 1)
          :let [m (dec n)
                q [(- (* n x1) (* m x2)) (- (* n y1) (* m y2))]]
          :while (in-range? q)]
      q)
    (for [n (iterate inc 1)
          :let [m (dec n)
          q [(- (* n x2) (* m x1)) (- (* n y2) (* m y1))]]
          :while (in-range? q)]
      q))))

(time
 (let [input (slurp "src/day08/input.txt")
       width (.indexOf input "\n")
       height (dec (/ (count input) width))
       in-range? (in-range?-fn width height)
       antennae
       (->> (str/replace input #"\n" "")
            (map-indexed vector)
            (reduce
             (fn [m [idx character]]
               (if (= \. character)
                 m
                 (let [[x y] [(mod idx height) (quot idx width)]]
                   (update m character conj [x y]))))
             {}))
       antinodes
       (->> antennae
            (reduce
            (fn [acc [_ points]]
              (let [pairs (combo/combinations points 2)]
                (reduce
                 (fn [coll [p1 p2]]
                   (into coll (calculate-antipodes* in-range? p1 p2)))
                 acc
                 pairs))) 
             #{}))]
   (count antinodes)))
