(ns day19.solution 
  (:require
    [clojure.string :as str]))

;; solution part 1

(time
 (let [[available-towel-patterns desired-designs]
       ((fn [[a b]] [(into #{} (str/split a #", ")) (str/split-lines b)])
        (str/split (slurp "src/day19/input.txt") #"\n\n"))
       pattern-regex (re-pattern (str "^(" (str/join "|" available-towel-patterns) ")*$"))]
   (->> desired-designs
        (keep (partial re-matches pattern-regex))
        count)))

;; solution part 2

(def combinations
  (memoize 
   (fn
     ([substrings target]
      (combinations substrings target 1))
     ([substrings target acc]
      (if (empty? target)
        acc
        (->> (for [sub substrings
                   :when (str/starts-with? target sub)
                   :let [target' (subs target (count sub))]]
               (* acc
                  (combinations substrings target' acc)))
             (apply +)))))))

(time
 (let [[available-towel-patterns desired-designs]
       ((fn [[a b]] [(into #{} (str/split a #", ")) (str/split-lines b)])
        (str/split (slurp "src/day19/input.txt") #"\n\n"))
       pattern-regex (re-pattern (str "^(" (str/join "|" available-towel-patterns) ")*$"))]
   (->> desired-designs
        (filter (partial re-matches pattern-regex))
        (map (partial combinations available-towel-patterns))
        (apply +))))
