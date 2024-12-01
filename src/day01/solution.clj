(ns day01.solution)

(defn- difference [x y]
  (abs (- x y)))

;; solution part 1

(time
 (let [[left right] (->> "src/day01/input.txt"
                         slurp
                         (re-seq #"\d+")
                         (map Integer/new)
                         (partition 2)
                         (apply map list))]
   (apply + (map difference (sort left) (sort right)))))

;; solution part 2

(time
 (let [[left right] (->> "src/day01/input.txt"
                         slurp
                         (re-seq #"\d+")
                         (map Integer/new)
                         (partition 2)
                         (apply map list))
       freq (frequencies right)]
   (apply + (map * left (map #(get freq % 0) left)))))
