(ns day04.solution 
  (:require
   [clojure.string :as str]))

(defn- transpose [matrix]
  (map str/join (apply mapv vector matrix)))

(defn- diagonals [matrix]
  (let [rows (count matrix)
        cols (count (first matrix))]
    (for [d (range (- (dec rows)) cols)]
      (let [row-start (max 0 (- d))
            col-start (max 0 d)]
        (str/join
         (for [i (range (min (- rows row-start)
                             (- cols col-start)))]
           (get-in matrix [(+ row-start i) (+ col-start i)])))))))

(defn- anti-diagonals [matrix]
  (let [rows (count matrix)
        cols (count (first matrix))]
    (for [d (range (- (dec rows)) cols)]
      (let [start (max 0 d)
            end (min rows (+ rows d))
            counting-up (range start end)
            counting-down (reverse counting-up)]
        (str/join
         (map (fn [x y] (get-in matrix [x y]))
              counting-up
              counting-down))))))

;; solution part 1

(defn- re-count [re s]
  (count (re-seq re s)))

(defn- count-horizontal [input]
  (transduce (map #(re-count #"(?=XMAS)|(?=SAMX)" (apply str %))) + 0 input))

(time
 (let [input (str/split-lines (slurp "src/day04/input.txt"))]
   (+ (count-horizontal input)
      (count-horizontal (transpose input))      
      (count-horizontal (diagonals input))
      (count-horizontal (anti-diagonals input)))))
      
;; solution part 2

(time
 (let [input (str/split-lines (slurp "src/day04/input.txt"))
       rows (count input)
       cols (count (first input))]
   (count
    (for [x (range rows)
          y (range cols)
          :let [point [y x]
                c (get-in input [y x])]
          :when (= \A c)
          :let [candidate
                (for [delta [[-1 -1] ;; top-left
                             [1 -1] ;; bottom-left
                             [-1  1] ;; top-right
                             [1  1]];; bottom-right
                      :let [p (map + point delta)]
                      :when (< -1 (first p) cols)
                      :when (< -1 (last p) rows)]
                  (get-in input p))]
          :when (and (= {\S 2 \M 2} (frequencies candidate))
                     (not (#{'(\M \S \S \M)
                             '(\S \M \M \S)} candidate)))]
      [x y]))))
