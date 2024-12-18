(ns day18.solution
  (:require
   [astar :refer [a*]]
   [clojure.string :as str]))

;; solution part 1

(defn- manhattan-distance [[x y] [x' y']]
  (+ (abs (- x x'))
     (abs (- y y'))))

(time
 (let [input (map (comp #(mapv Integer/new %) #(str/split % #",")) (str/split-lines (slurp "src/day18/input.txt")))
       length 70
       bytes 1024
       corrupted (into #{} (take bytes input))
       graph (->> (for [x (range (inc length))
                        y (range (inc length))
                        :when (not (contains? corrupted [x y]))]
                    {[x y]
                     (->> (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
                                :let [x' (+ x dx)
                                      y' (+ y dy)]
                                :when (< -1 x' (+ 2 length))
                                :when (< -1 y' (+ 2 length))
                                :when (not (contains? corrupted [x' y']))]
                            [x' y'])
                          (into #{}))})
                  (into {}))]
   (dec (count (a* graph [0 0] [length length] manhattan-distance (constantly 0))))))

;; solution part 2

(time ; ~5 seconds
 (let [input (mapv (comp #(mapv Integer/new %) #(str/split % #",")) (str/split-lines (slurp "src/day18/input.txt")))
       length 70
       init-bytes 1024
       init-corrupted (set (take init-bytes input))
       init-graph (->> (for [x (range (inc length))
                             y (range (inc length))
                             :when (not (contains? init-corrupted [x y]))]
                         {[x y]
                          (->> (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
                                     :let [x' (+ x dx)
                                           y' (+ y dy)]
                                     :when (< -1 x' (+ 2 length))
                                     :when (< -1 y' (+ 2 length))
                                     :when (not (contains? init-corrupted [x' y']))]
                                 [x' y'])
                               (into #{}))})
                       (into {}))
       init-shortest-path (set (a* init-graph [0 0] [length length] manhattan-distance (constantly 0)))
       last-byte
       (loop [bytes init-bytes
              graph init-graph
              shortest-path init-shortest-path]
         (if (empty? shortest-path)
           bytes
           (let [bytes' (inc bytes)
                 coord (get input bytes')
                 graph' (-> graph
                            (dissoc coord)
                            (update-vals #(into #{} (remove #{coord}) %)))]
             (if (contains? shortest-path coord)
               (recur bytes' graph' (set (a* graph' [0 0] [length length] manhattan-distance (constantly 0))))
               (recur (inc bytes) graph' shortest-path)))))]
   (get input last-byte)))
