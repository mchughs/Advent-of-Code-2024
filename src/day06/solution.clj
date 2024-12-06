(ns day06.solution
  (:require
   [clojure.string :as str]))

;; solution part 1

(defn- out-of-bounds-fn? [width height]
  (fn [[x y]]
    (or (not (< -1 x width))
        (not (< -1 y height)))))

(defn- rotate-90-clockwise [[x y]]
  [(- y) x])

(time
 (let [input (->> "src/day06/input.txt"
                  slurp
                  str/split-lines)
       width (count (first input))
       height (count input)
       out-of-bounds? (out-of-bounds-fn? width height)
       idx (.indexOf (str/join input) "^")
       start [(mod idx height) (quot idx width)]]
   (loop [position start
          direction [0 -1]
          visited #{start}]
     (if (out-of-bounds? position)
       (dec (count visited))
       (let [[x y :as position'] (mapv + position direction)
             blocked? (= \# (get-in input [y x]))]
         (if blocked?
           (recur position (rotate-90-clockwise direction) visited)
           (recur position' direction (conj visited position'))))))))

;; solution part 2

(time
 (let [input (->> "src/day06/input.txt"
                  slurp
                  str/split-lines)
       width (count (first input))
       height (count input)
       out-of-bounds? (out-of-bounds-fn? width height)
       idx (.indexOf (str/join input) "^")
       start [(mod idx height) (quot idx width)]
       input (mapv #(mapv identity %) input)
       path
       (loop [position start
              direction [0 -1]
              visited [[start direction]]]
         (if (out-of-bounds? position)
           visited
           (let [[x y :as position'] (mapv + position direction)
                 blocked? (= \# (get-in input [y x]))]
             (if blocked?
               (recur position (rotate-90-clockwise direction) visited)
               (recur position' direction (conj visited [position' direction]))))))
       identify-loop-fn
       (fn [[[a b] _]]
         ;; Currently quite slow
         ;; We could likely have a big speed up by starting the loop from the point 
         ;; immediately preceding the new obstacle.
         (loop [position start
                direction [0 -1]
                visited #{[start direction]}]
           (if (out-of-bounds? position)
             nil
             (let [[x y :as position'] (mapv + position direction)]
               (if (contains? visited [position' direction])
                 [a b]
                 (let [blocked? (or (= \# (get-in input [y x]))
                                    (= [x y] [a b]))]
                   (if blocked?
                     (recur position (rotate-90-clockwise direction) visited)
                     (recur position' direction (conj visited [position' direction])))))))))]
   (->> path
        rest
        (keep identify-loop-fn)
        (into #{})
        count)))
