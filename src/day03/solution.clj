(ns day03.solution)

;; solution part 1

(time
 (->> "src/day03/input.txt"
      slurp
      (re-seq #"mul\((\d+),(\d+)\)")
      (transduce
       (map (fn [[_ a b]]
              (* (Integer/new a)
                 (Integer/new b))))
       +
       0)))

;; solution part 2

(time
 (->> "src/day03/input.txt"
      slurp
      (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)")
      (reduce 
       (fn [acc [x a b]]
         (cond
           (= x "don't()") (assoc acc :active? false)
           (= x "do()") (assoc acc :active? true)
           (:active? acc) (update acc :sum + (* (Integer/new a)
                                                (Integer/new b)))
           :else acc))
       {:sum 0 :active? true})
      :sum))
