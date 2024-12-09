(ns day09.solution)

;; solution part 1

(time
 (let [input (->> "src/day09/input.txt"
                  slurp
                  butlast
                  (map (comp Integer/new str)))
       disk
       (->> (concat input '(0))
            (partition-all 2)
            (map-indexed vector))]
   (loop [acc 0
          free 0
          idx 0
          [x & xs :as remaining] disk]
     (cond
       (nil? x)
       acc

       (pos? free)
       (let [[n [file _]] (last remaining)
             diff (- free file)]
         (if (pos? diff)
           (recur
            (reduce + acc (map * (iterate inc idx) (repeat file n)))
            diff
            (+ idx file)
            (butlast remaining))
           (recur
            (reduce + acc (map * (iterate inc idx) (repeat free n)))
            0
            (+ idx free)
            (concat (butlast remaining)
                    (list [n (list (- diff) 0)])))))

       :else
       (let [[n [file z]] x]
         (recur (reduce + acc (map * (iterate inc idx) (repeat file n)))
                z
                (+ idx file)
                xs))))))

;; solution part 2

(defn- split-at-indices
  [coll idx1 idx2]
  (let [[idx1 idx2] (sort [idx1 idx2])
        [first-split remaining] (split-at idx1 coll)
        [second-split third-split] (split-at (- idx2 idx1) remaining)]
    [first-split second-split third-split]))

(defn- move
  [coll [id [file _]]]
  (let [curr-idx (->> coll
                      (keep-indexed (fn [idx [id' _]]
                                      (when (= id id')
                                        idx)))
                      first)
        target-idx (->> coll
                        (keep-indexed (fn [idx [_ [_ free]]]
                                        (when (>= free file)
                                          idx)))
                        first)]
    (cond
      (or (nil? curr-idx) (nil? target-idx))
      coll

      (<= curr-idx target-idx)
      coll

      :else
      (let [[first-split second-split third-split] (split-at-indices coll (inc target-idx) curr-idx)
            [id' [file' free'] :as _new-predecessor] (last first-split)
            [id'' [file'' free''] :as old-predecessor] (last second-split)
            [_ [file* free*]] (first third-split)]
        (cond
          ;; Move over without displacing any files
          (nil? old-predecessor)
          (concat (butlast first-split)
                  (list [id' [file' 0]]
                        [id [file (+ free* free')]])
                  (rest third-split))

          :else
          (doall ;; avoid stack overflow error
           (concat (butlast first-split)
                   (list [id' [file' 0]]
                         [id [file (- free' file)]])
                   (butlast second-split)
                   (list [id'' [file'' (+ free'' file* free*)]])
                   (rest third-split))))))))

(time
 (let [input (->> "src/day09/input.txt"
                  slurp
                  butlast
                  (map (comp Integer/new str)))
       disk (->> (concat input '(0))
                 (partition-all 2)
                 (map-indexed vector))
       arranged-disk (reduce move disk (reverse disk))]
   (->> arranged-disk
        (reduce
         (fn [{:keys [sum idx]} [id [n m]]]
           {:sum (reduce + sum (map * (iterate inc idx) (repeat n id)))
            :idx (+ idx n m)})
         {:sum 0 :idx 0})
        :sum)))
