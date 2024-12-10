(ns day10.solution 
  (:require
   [clojure.string :as str]))

(defn- in-range?-fn [width height]
  (fn [idx]
    (< -1 idx (* width height))))

;; solution part 1

(defn- bfs
  [graph score-fn start]
  (loop [coll (conj clojure.lang.PersistentQueue/EMPTY start)
         visited #{}
         score 0]
    (cond
      (empty? coll) score
      (visited (peek coll)) (recur (pop coll) visited score)
      :else (let [curr (peek coll)
                  score (if (score-fn curr) (inc score) score)
                  node (graph curr)
                  coll (into (pop coll) node)
                  visited (conj visited curr)]
              (recur coll visited score)))))

(time
 (let [input (slurp "src/day10/input.txt")
       width (.indexOf input "\n")
       height (dec (/ (count input) width))
       in-range? (in-range?-fn width height)
       input (str/replace input #"\n" "")
       trail-heads (keep-indexed (fn [idx item] (when (#{\0} item) idx)) input)
       valid-step (fn [loc]
                    (let [x (mod loc height)]
                      (for [delta [(- width)
                                   width
                                   ;; prevent scrolling off the map left or right
                                   (when-not (zero? x) -1)
                                   (when-not (= height (inc x)) 1)]
                            :when (not (nil? delta))
                            :let [loc' (+ loc delta)]
                            :when (in-range? loc')
                            :when (= (int (get input loc'))
                                     (inc (int (get input loc))))]
                        loc')))]
   (transduce (map (partial bfs valid-step #(= \9 (get input %))))
              +
              0
              trail-heads)))

;; solution part 2

(defn- bfs*
  [graph score-fn start]
  (loop [coll (conj clojure.lang.PersistentQueue/EMPTY start)
         visited #{}
         score 0]
    ;; The same but without skipping over already visited paths 
    (if (empty? coll)
      score
      (let [curr (peek coll)
            score (if (score-fn curr) (inc score) score)
            node (graph curr)
            coll (into (pop coll) node)
            visited (conj visited curr)]
        (recur coll visited score)))))

(time
 (let [input (slurp "src/day10/input.txt")
       width (.indexOf input "\n")
       height (dec (/ (count input) width))
       in-range? (in-range?-fn width height)
       input (str/replace input #"\n" "")
       trail-heads (keep-indexed (fn [idx item] (when (#{\0} item) idx)) input)
       valid-step (fn [loc]
                    (let [x (mod loc height)]
                      (for [delta [(- width)
                                   width
                                   ;; prevent scrolling off the map left or right
                                   (when-not (zero? x) -1)
                                   (when-not (= height (inc x)) 1)]
                            :when (not (nil? delta))
                            :let [loc' (+ loc delta)]
                            :when (in-range? loc')
                            :when (= (int (get input loc'))
                                     (inc (int (get input loc))))]
                        loc')))]
   (transduce (map (partial bfs* valid-step #(= \9 (get input %))))
              +
              0
              trail-heads)))
