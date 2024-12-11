(ns day11.solution 
  (:require
   [com.walmartlabs.cond-let :refer [cond-let]]))

;; solution part 1

(defn- conj*
  ([] '())
  ([x] (conj x))
  ([coll n]
   (if (seq? n)
     (into coll n)
     (conj coll n))))

#_:clj-kondo/ignore
(def rule
  (memoize
   (fn [^long n]
     (cond-let
      (zero? n) 1

      :let [s (str n)
            length (count s)]
      (even? length)
      (list (Long/new (subs s 0 (/ length 2)))
            (Long/new (subs s (/ length 2))))

      :else
      (* n 2024)))))

(defn- blink [coll]
  (transduce (map rule) conj* coll))

;; Slow, naive approach
(time
 (let [input (map Integer/new (re-seq #"\d+" (slurp "src/day11/input.txt")))
       target 25]
   (->> input
        (iterate blink)
        (take (inc target))
        last
        count)))

;; solution part 2

(def +* (fnil + 0))

(defn- blink* [m]
  (reduce-kv
   (fn [acc k v]
     (let [x (rule k)]
       (if (seq? x)
         (let [[a b] x]
           (-> acc
               (update a +* v)
               (update b +* v)))
         (update acc x +* v))))
   {}
   m))

;; Faster approach since there are many duplicate stones and we don't need to preserve their order.
(time
 (let [input (frequencies (map Integer/new (re-seq #"\d+" (slurp "src/day11/input.txt"))))
       target 75]   
   (->> input
        (iterate blink*)
        (take (inc target))
        last
        vals
        (apply +))))
