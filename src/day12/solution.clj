(ns day12.solution
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(defn- neighbor-fn 
  [grid char width height]
  (fn [[x y]]
    (for [i [(dec y) y (inc y)]
          j [(dec x) x (inc x)]
          :when (and (< -1 i height)
                     (< -1 j width)
                     (or (= x j) (= y i))
                     (not (and (= x j) (= y i)))
                     (= char (get-in grid [i j])))]
      [j i])))

(defn- bfs
  [graph start]
  (loop [coll (conj clojure.lang.PersistentQueue/EMPTY start)
         visited #{}]
    (cond
      (empty? coll) visited
      (visited (peek coll)) (recur (pop coll) visited)
      :else (let [curr (peek coll)
                  node (graph curr)
                  coll (into (pop coll) node)
                  visited (conj visited curr)]
              (recur coll visited)))))

(defn- flood-fill-fn [grid width height]
  (fn [[x y :as start]]
    (let [char (get-in grid [y x])
          graph (neighbor-fn grid char width height)]
      (bfs graph start))))

;; solution part 1

(defn- calc-perimeter [region]
  (transduce
   (map (fn [[x y]]
          (count
           (for [[x' y'] [[x (dec y)]
                          [x (inc y)]
                          [(inc x) y]
                          [(dec x) y]]
                 :when (not (contains? region [x' y']))]
             [x' y']))))
   +
   0
   region))

(time
 (let [input (slurp "src/day12/input.txt")
       grid (str/split-lines input)
       width (count (first grid))
       height (count grid)
       flood-fill (flood-fill-fn grid width height)
       regions (loop [regions #{}
                      points (set (for [x (range width)
                                        y (range height)]
                                    [x y]))]
                 (if (empty? points)
                   regions
                   (let [start (first points)
                         region (flood-fill start)]
                     (recur (conj regions region)
                            (set/difference points region)))))]
   (reduce 
    (fn [acc region]
      (let [area (count region)
            perimeter (calc-perimeter region)]
        (+ acc (* area perimeter))))
    0
    regions)))

;; solution part 2

(defn- calc-perimeter* [region]
  (let [edges (into #{}
                    (mapcat
                     (fn [[x y]]
                       (for [[x' y' polarity] [[x (dec y) :up]
                                               [x (inc y) :down]
                                               [(inc x) y :right]
                                               [(dec x) y :left]]
                             :when (not (contains? region [x' y']))]
                         [x' y' polarity])))
                    region)]
    (reduce
     (fn [acc [x y polarity]]
       (condp contains? polarity
         #{:up :down}
         (if (contains? edges [(dec x) y polarity])
           acc
           (inc acc))
         #{:left :right}
         (if (contains? edges [x (dec y) polarity])
           acc
           (inc acc))))
     0
     edges)))

(time
 (let [input (slurp "src/day12/input.txt")
       grid (str/split-lines input)
       width (count (first grid))
       height (count grid)
       flood-fill (flood-fill-fn grid width height)
       regions (loop [regions #{}
                      points (set (for [x (range width)
                                        y (range height)]
                                    [x y]))]
                 (if (empty? points)
                   regions
                   (let [start (first points)
                         region (flood-fill start)]
                     (recur (conj regions region)
                            (set/difference points region)))))]
   (reduce
    (fn [acc region]
      (let [area (count region)
            perimeter (calc-perimeter* region)]
        (+ acc (* area perimeter))))
    0
    regions)))
