(ns day20.solution
  (:require
   [astar :refer [a*]]
   [clojure.string :as str]))

;; solution part 1
 
(time
 (let [input (str/split-lines (slurp "src/day20/input.txt"))
       [max-x max-y] ((juxt (comp count first) count) input)
       tiles (into {}
                   (for [x (range max-x)
                         y (range max-y)]
                     [[x y] (get-in input [y x])]))
       accessible (into #{}
                        (comp (remove (fn [[_ v]] (= \# v)))
                              (map first))
                        tiles)
       start (ffirst (filter (fn [[_ v]] (= \S v)) tiles))
       end (ffirst (filter (fn [[_ v]] (= \E v)) tiles))
       graph (->>
              (for [[x y] accessible]
                {[x y] (->>
                        (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]]
                          [(+ x dx) (+ y dy)])
                        (into #{}))})
              (into {}))
       path (a* graph start end (constantly 1) (constantly 0))
       points->idx (into {}
                         (map-indexed (fn [idx point] [point idx]))
                         path)]   
   (->> path
        (map-indexed (fn [idx [x y]]
                       (for [[dx dy] [[-2 0] [2 0] [0 -2] [0 2]]
                             :let [[x' y'] [(+ x dx) (+ y dy)]
                                   idx' (get points->idx [x' y'] 0)]
                             :when (< idx idx')]
                         (- idx' idx 2))))
        flatten
        (remove (partial > 100))
        count)))

;; solution part 2

(defn- manhattan-distance [[x y] [x' y']]
  (+ (abs (- x x'))
     (abs (- y y'))))

;; ~10 seconds
(time
 (let [input (str/split-lines (slurp "src/day20/input.txt"))
       [max-x max-y] ((juxt (comp count first) count) input)
       tiles (into {}
                   (for [x (range max-x)
                         y (range max-y)]
                     [[x y] (get-in input [y x])]))
       accessible (into #{}
                        (comp (remove (fn [[_ v]] (= \# v)))
                              (map first))
                        tiles)
       start (ffirst (filter (fn [[_ v]] (= \S v)) tiles))
       end (ffirst (filter (fn [[_ v]] (= \E v)) tiles))
       graph (->>
              (for [[x y] accessible]
                {[x y] (->>
                        (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]]
                          [(+ x dx) (+ y dy)])
                        (into #{}))})
              (into {}))
       path (a* graph start end (constantly 1) (constantly 0))
       points->idx (into {}
                         (map-indexed (fn [idx point] [point idx]))
                         path)]
   (->> points->idx
        (mapcat (fn [[[x y] idx]]
                  (for [dx (range -20 21)
                        dy (range -20 21)
                        :let [[x' y'] [(+ x dx) (+ y dy)]]
                        :when (contains? accessible [x' y'])
                        :let [idx' (get points->idx [x' y'] 0)]
                        :when (< idx idx')
                        :let [d (manhattan-distance [x y] [x' y'])]
                        :when (>= 20 d)
                        :let [time-saved (- idx' idx d)]
                        :when (pos? time-saved)]
                    time-saved)))
        (remove (partial > 100))
        count)))
