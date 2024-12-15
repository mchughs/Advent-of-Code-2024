(ns day15.solution 
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

;; solution part 1

(defn- push-box
  ([grid position move]
   (push-box grid position move 1))
  ([grid position move n]
   (let [position' (+ position move)
         preview (get grid position')]
     (case preview
       \. n
       \# nil
       \O (push-box grid position' move (inc n))))))

(defn- step [grid position move]
  (let [position' (+ position move)
        preview (get grid position')]
    (case preview
      \. [position' (-> grid
                        (assoc position \.)
                        (assoc position' \@))]
      \# [position grid]
      \O (if-let [n (push-box grid position' move)]
           [position' (-> grid
                          (assoc position \.)
                          (assoc position' \@)
                          (assoc (+ position' (* n move)) \O))]
           [position grid]))))

(time
 (let [[grid-str moves-str] (str/split (slurp "src/day15/input.txt") #"\n\n")
       input (str/split-lines grid-str)
       width (count (first input))
       moves (map {\< -1
                   \> 1
                   \^ (- width)
                   \v width}
                  (str/replace moves-str #"\n" ""))
       seed-grid (mapv identity (str/replace grid-str  #"\n" ""))
       start (first (keep-indexed #(when (#{\@} %2) %1) seed-grid))
       final-grid
       (loop [position start
              grid seed-grid
              [move & remaining] moves]
         (if move
           (let [[position' grid'] (step grid position move)]
             (recur position' grid' remaining))
           grid))]
   (->> final-grid
        (keep-indexed #(when (#{\O} %2) %1))
        (reduce #(+ %1
                    (* 100 (quot %2 width))
                    (mod %2 width))
                0))))

;; solution part 2

(defn- push-box-horizontal
  ([grid position move]
   (push-box-horizontal grid position move 2))
  ([grid position move n]
   (let [position' (+ position move)
         preview (get grid position')]
     (condp contains? preview
       #{\.} n
       #{\#} nil
       ;; since boxes are 2 wide
       #{\[ \]} (push-box-horizontal grid position' move (+ n 2))))))

(defn- push-box-vertical
  [grid position move]
  (let [position' (+ position move)
        preview (get grid position')]
    (case preview
      \. #{position}
      \# nil
      \] (let [position'' (dec position')
               right (push-box-vertical grid position' move)
               left (push-box-vertical grid position'' move)]
           (when (and left right)
             (set/union #{position' position''}
                        left
                        right)))
      \[ (let [position'' (inc position')
               left (push-box-vertical grid position' move)
               right (push-box-vertical grid position'' move)]
           (when (and left right)
             (set/union #{position' position''}
                        left
                        right))))))

(defn- step* [grid position move]
  (let [position' (+ position move)
        preview (get grid position')
        horizontal? (#{-1 1} move)]
    (cond
      (= \. preview)
      [position' (-> grid
                     (assoc position \.)
                     (assoc position' \@))]

      (= \# preview)
      [position grid]

      (and (#{\[ \]} preview) horizontal?)
      (if-let [n (push-box-horizontal grid position' (* 2 move))]
        (let [idx position'
              idx' (+ (* n move) idx)
              start (min idx idx')
              end (max idx idx')
              start (if (pos? move) (inc start) start)
              end (if (pos? move) (inc end) end)
              boxes (mapv identity (str/join (repeat (/ n 2) "[]")))
              grid' (-> (subvec grid 0 start)
                        (into boxes)
                        (into (subvec grid end)))]
          [position' (-> grid'
                         (assoc position \.)
                         (assoc position' \@))])

        [position grid])
      
      (#{\[ \]} preview)
      (if-let [places (push-box-vertical grid position move)]
        (let [f (if (pos? move) reverse identity) ;; move the boxes in the proper order
              grid' (reduce
                     (fn [acc loc]
                       (-> acc
                           (assoc loc \.)
                           (assoc (+ move loc)
                                  (get acc loc))))
                     grid
                     (f (sort places)))]
          [position' (-> grid'
                         (assoc position \.)
                         (assoc position' \@))])
        [position grid]))))

(def widen
  {\# "##"
   \O "[]"
   \. ".."
   \@ "@."})

(time
 (let [[grid-str moves-str] (str/split (str/join (map #(if-let [x (widen %)] x %)
                                                      (slurp "src/day15/input.txt")))
                                       #"\n\n")
       input (str/split-lines grid-str)
       width (count (first input))
       moves (map {\< -1
                   \> 1
                   \^ (- width)
                   \v width}
                  (str/replace moves-str #"\n" ""))
       seed-grid (mapv identity (str/replace grid-str  #"\n" ""))
       start (first (keep-indexed #(when (#{\@} %2) %1) seed-grid))
       final-grid
       (loop [position start
              grid seed-grid
              [move & remaining] moves]
         (if move
           (let [[position' grid'] (step* grid position move)]
             (recur position' grid' remaining))
           grid))]
   (->> final-grid
        (keep-indexed #(when (#{\[} %2) %1))
        (reduce #(+ %1
                    (* 100 (quot %2 width))
                    (mod %2 width))
                0))))
