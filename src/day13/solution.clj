(ns day13.solution 
  (:require
    [clojure.string :as str]))

(defn- determinant [[u1 u2] [w1 w2]]
  (- (* u1 w2)
     (* u2 w1)))

;; solution part 1

(time
 (let [input (map (comp #(partition 2 %)
                        #(map parse-long %)
                        #(re-seq #"\d+" %))
                  (str/split (slurp "src/day13/input.txt") #"\n\n"))]
   (reduce
    (fn [acc [A B target]]
      ;; Cramer's rule
      (let [det (determinant A B)
            a (/ (determinant target B) det)
            b (/ (determinant A target) det)]
        (if (and (pos-int? a)
                 (pos-int? b))
          (+ acc (* 3 a) b)
          acc)))
    0
    input)))

;; solution part 2

(time
 (let [input (map (comp #(partition 2 %)
                        #(map parse-long %)
                        #(re-seq #"\d+" %))
                  (str/split (slurp "src/day13/input.txt") #"\n\n"))]
   (reduce
    (fn [acc [A B target]]
      ;; Cramer's rule
      (let [target (map + target (repeat 10000000000000))
            det (determinant A B)
            a (/ (determinant target B) det)
            b (/ (determinant A target) det)]
        (if (and (pos-int? a)
                 (pos-int? b))
          (+ acc (* 3 a) b)
          acc)))
    0
    input)))
