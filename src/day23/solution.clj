(ns day23.solution
  (:require
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]))

;; solution part 1

(defn- fully-connected? [connections coll]
  (let [size (count coll)
        s (set coll)]
    (->> coll
         (map #(set/intersection (get connections %) s))
         (every? #(= (dec size) (count %))))))

(defn- networks [size connections]
  (let [computers (keys connections)]
    (->> computers
         (mapcat (fn [computer]
                   (let [network? (get connections computer)]
                     (for [coll (map #(conj % computer) (combo/combinations network? (dec size)))
                           :when (fully-connected? connections coll)]
                       (set coll)))))
         (into #{}))))

(time
 (let [input (map #(str/split % #"\-") (str/split-lines (slurp "src/day23/input.txt")))]
   (->> input
        (reduce (fn [m [a b]]
                  (-> m
                      (update a (fnil conj #{}) b)
                      (update b (fnil conj #{}) a)))
                {})
        (networks 3)
        (filter (fn [coll] (some #(str/starts-with? % "t") coll)))
        count)))

;; solution part 2

(defn- largest-network [computer connections]
  (let [computers (get connections computer)
        possible-network (conj computers computer)]
    (loop [next-size (dec (count computers))
           colls [possible-network]]
      (if (empty? colls)
        #{}
        (if-let [coll (first (filter #(fully-connected? connections %) colls))]
          (set coll)
          (recur (dec next-size)
                 (map #(conj % computer)
                      (combo/combinations computers next-size))))))))

(time
 (let [input (map #(str/split % #"\-") (str/split-lines (slurp "src/day23/input.txt")))
       connections (reduce (fn [m [a b]]
                             (-> m
                                 (update a (fnil conj #{}) b)
                                 (update b (fnil conj #{}) a)))
                           {}
                           input)]
   (->> connections
        keys
        (map #(largest-network % connections))
        (into #{})
        (reduce (fn [longest current]
                  (if (> (count current) (count longest))
                    current
                    longest)))
        (sort)
        (str/join ","))))
