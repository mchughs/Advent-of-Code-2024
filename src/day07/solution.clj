(ns day07.solution
  (:require
   [clojure.java.io :as io]
   [com.walmartlabs.cond-let :refer [cond-let]]))

;; solution part 1

(defn- operable?
  ([[x & xs]]
   (operable? x xs))
  ([x xs]
   (cond-let
    (empty? xs) false
    (= (list x) xs) true
    :let [a (/ x (last xs))]
    (and (int? a)
         (operable? a (butlast xs))) true
    :let [b (- x (last xs))]
    (and (pos? b)
         (operable? b (butlast xs))) true
    :else false)))

(time
 (with-open [rdr (io/reader "src/day07/input.txt")]
   (->> rdr
        line-seq
        (transduce
         (comp
          (map #(->> %
                     (re-seq #"\d+")
                     (map Long/new)))
          (filter operable?)
          (map first))
         +
         0))))

;; solution part 2

(defn- split-at-last-n
  "Splits a string into two parts: the part before the last `n` characters and the last `n` characters."
  [s n]
  (let [split-index (max 0 (- (count s) n))]
    [(subs s 0 split-index) (subs s split-index)]))

(defn- operable?*
  ([[x & xs]]
   (operable?* x xs))
  ([x xs]
   (cond-let
    (empty? xs) false
    (= (list x) xs) true
    :let [l (last xs)
          a (/ x l)]
    (and (int? a)
         (operable?* a (butlast xs))) true
    :let [b (- x l)]
    (and (pos? b)
         (operable?* b (butlast xs))) true
    :let [digits (count (str l))
          [c x'] (split-at-last-n (str x) digits)]
    (and (= x' (str l))
         (seq c)
         (operable?* (Long/new c) (butlast xs))) true
    :else false)))

(time
 (with-open [rdr (io/reader "src/day07/input.txt")]
   (->> rdr
        line-seq
        (transduce
         (comp
          (map #(->> %
                     (re-seq #"\d+")
                     (map Long/new)))
          (filter operable?*)
          (map first))
         +
         0))))
