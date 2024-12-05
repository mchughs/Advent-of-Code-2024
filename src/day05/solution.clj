(ns day05.solution
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

;; solution part 1

(defn- valid? [page-orderings page-list]
  (loop [[x & xs] page-list]
    (let [before (page-orderings x)]
      (cond
        (empty? xs)
        true
        (empty? (set/intersection before (set xs)))
        (recur xs)
        :else
        false))))

(defn- middle [coll]
  (nth coll (quot (count coll) 2)))

(time
 (let [[page-orderings* _ pages*] (->> "src/day05/input.txt"
                                       slurp
                                       str/split-lines
                                       (partition-by empty?))
       page-orderings (->> page-orderings*
                           (map #(map Integer/new (str/split % #"\|")))
                           (reduce (fn [m [k v]]
                                     (update m v #(if (nil? %1)
                                                    #{%2}
                                                    (conj %1 %2)) k))
                                   ;; values indicate pages that must precede the key
                                   {}))
       page-lists (->> pages*
                       (map #(map Integer/new (str/split % #"\,"))))]
   (transduce
    (comp (filter #(valid? page-orderings %))
          (map middle))
    +
    0
    page-lists)))

;; solution part 2

(time
 (let [[page-orderings* _ pages*] (->> "src/day05/input.txt"
                                       slurp
                                       str/split-lines
                                       (partition-by empty?))
       page-orderings (->> page-orderings*
                           (map #(map Integer/new (str/split % #"\|")))
                           (reduce (fn [m [k v]]
                                     (update m v #(if (nil? %1)
                                                    #{%2}
                                                    (conj %1 %2)) k))
                                   ;; values indicate pages that must precede the key
                                   {}))
       page-lists (->> pages*
                       (map #(map Integer/new (str/split % #"\,"))))]
   (transduce
    (comp (remove #(valid? page-orderings %))
          (map #(sort (fn [a b] (contains? (page-orderings b) a)) %))
          (map middle))
    +
    0
    page-lists)))

