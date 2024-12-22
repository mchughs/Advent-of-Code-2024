(ns day22.solution 
  (:require
   [clojure.string :as str]))

;; solution part 1

(def mix bit-xor)
(def prune #(mod % 16777216))

(defn- evolve [n]
  (let [n (prune (mix n (* n 64)))
        n (prune (mix n (quot n 32)))
        n (prune (mix n (* n 2048)))]
    n))

(time
 (let [input (map parse-long (str/split-lines (slurp "src/day22/input.txt")))]   
   (transduce
    (map #(nth (iterate evolve %) 2000))
    + 0
    input)))

;; solution part 2

(defn- diff [[a b]]
  (- b a))

;; slow, ~2 minutes
(time
 (let [input (map parse-long (str/split-lines (slurp "src/day22/input.txt")))
       sequences->prices
       (->> input
            (map (fn [n]
                   (let [prices (->> n (iterate evolve) (take 2001) (map #(mod % 10)))
                         price-diff-sequences (->> prices (partition 2 1) (map diff) (partition 4 1))]
                     (->> (map vector price-diff-sequences (drop 4 prices))
                          (reduce
                           (fn [m [price-diff-sequence price]]
                             (if (get m price-diff-sequence)
                               m
                               (assoc m price-diff-sequence price)))
                           {}))))))
       sequences (into #{} (mapcat keys) sequences->prices)
       sequence->bananas (fn [sequence]
                           (transduce
                            (map #(get % sequence 0))
                            +
                            0
                            sequences->prices))]
   (transduce
    (map sequence->bananas)
    max
    0
    sequences)))
