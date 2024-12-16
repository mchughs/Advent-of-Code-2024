(ns day16.solution 
  (:require
   [astar :refer [a*]]
   [clojure.string :as str]))

;; solution part 1

(defn build-graph [tiles]
  (let [accessible (into #{}
                         (comp (remove (fn [[_ v]] (= \# v)))
                               (map first))
                         tiles)
        directions [[-1 0] [1 0] [0 -1] [0 1]]]
    (->>
     (for [[x y] accessible
           [dx dy :as direction] directions
           :let [opposite-direction [(- dx) (- dy)]
                 [x' y'] [(+ x dx) (+ y dy)]]]
       {[x y dx dy]
        (into (if (contains? accessible [x' y'])
                #{[x' y' dx dy]}
                #{})
              (comp (remove #{direction opposite-direction})
                    (map (fn [[dx' dy']] [x y dx' dy'])))
              directions)})
     (into {}))))

(defn- cost [[_ _ dx dy] [_ _ dx' dy']]
  (if (and (= dx dx') (= dy dy'))
    1
    1000))

(defn- manhattan-distance [[x y _ _] [x' y' _ _]]
  (+ (abs (- x x'))
     (abs (- y y'))))

(time
 (let [input (str/split-lines (slurp "src/day16/input.txt"))
       [max-x max-y] ((juxt (comp count first) count) input)
       tiles (into {}
                   (for [x (range max-x)
                         y (range max-y)]
                     [[x y] (get-in input [y x])]))
       [x-start y-start] (ffirst (filter (fn [[_ v]] (= \S v)) tiles))
       [x-end y-end] (ffirst (filter (fn [[_ v]] (= \E v)) tiles))
       graph (build-graph tiles)
       start [x-start y-start 1 0] ;; start facing east
       end [x-end y-end 0 -1]] ;; might incur an extra cost of 1000 to face up on the last step
   (->>
    (a* graph start end cost manhattan-distance)
    (partition 2 1)
    (map #(apply cost %))
    (apply +))))

;; solution part 2

;; Answer is 428

;; Throwing in the towel for now. Should have been able to use the A* implemented here https://matthewdowney.github.io/astar-in-clojure-find-k-shortest-paths.html
;; For some reason it doesn't work (maybe because it gets caught in infinite loops?)
;; Or I'd need to figure out who to create a lazy seq of the shortest paths and just keep taking the paths while they share the min cost.
