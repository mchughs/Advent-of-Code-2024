(ns day14.solution
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; solution part 1

(defn- step-fn [width height]
  (fn [[x y vx vy]]
    [(mod (+ x vx) width)
     (mod (+ y vy) height)
     vx
     vy]))

(defn- quadrant-fn [width height]
  (fn [[x y _ _]]
    (let [xmid (quot width 2)
          ymid (quot height 2)]
      (cond
        (or (= x xmid) (= y ymid)) nil
        (and (< x xmid) (< y ymid)) :top-left
        (and (< x xmid) (> y ymid)) :bottom-left
        (and (> x xmid) (< y ymid)) :top-right
        (and (> x xmid) (> y ymid)) :bottom-right))))

(time
 (with-open [rdr (io/reader "src/day14/input.txt")]
   (let [width 101
         height 103
         seconds 100
         step (step-fn width height)
         quadrant (quadrant-fn width height)
         input (->> rdr
                    line-seq
                    (map (comp
                          #(map parse-long %)
                          #(re-seq #"-?\d+" %))))
         distribution (->> input
                           (map (comp quadrant
                                      last
                                      #(take (inc seconds) %)
                                      #(iterate step %)))
                           frequencies)]
     (apply * (vals (dissoc distribution nil))))))

;; solution part 2

(defn- display! [width height {:keys [robots idx]}]
  (let [points (into #{} (map (fn [[x y _ _]] [x y])) robots)]
    (println idx " seconds")
    (doall
     (for [y (range height)]
       (println
        (str/join
         (for [x (range width)]
           (if (contains? points [x y])
             \X
             \.))))))
    (println)))

(defn- step-fn* [width height]
  (fn [{:keys [robots idx]}]
    (let [robots'
          (map (fn [[x y vx vy]]
                 [(mod (+ x vx) width)
                  (mod (+ y vy) height)
                  vx
                  vy])
               robots)]
      {:robots robots'
       :idx (inc idx)})))

;; 8006 for my input
(time
 (with-open [rdr (io/reader "src/day14/input.txt")]
   (let [width 101
         height 103
         seed 26
         step (step-fn* width height)
         input (->> rdr
                    line-seq
                    (map (comp
                          #(map parse-long %)
                          #(re-seq #"-?\d+" %))))]
     (do
       (->> {:robots input :idx 0}
            (iterate step)
            (drop (inc seed))
            (take-nth 101)
            (take 100) ;; random guess
            (mapv #(display! width height %)))
       nil))))
