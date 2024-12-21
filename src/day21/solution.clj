(ns day21.solution
  (:require
   [astar :refer [bfs-shortest-paths]]
   [clojure.string :as str]))

;; solution part 1

(def number-pad
  {\A {\0 :left
       \3 :up}
   \0 {\A :right
       \2 :up}
   \1 {\4 :up
       \2 :right}
   \2 {\1 :left
       \5 :up
       \3 :right
       \0 :down}
   \3 {\2 :left
       \6 :up
       \A :down}
   \4 {\7 :up
       \5 :right
       \1 :down}
   \5 {\4 :left
       \8 :up
       \6 :right
       \2 :down}
   \6 {\9 :up
       \5 :left
       \3 :down}
   \7 {\4 :down
       \8 :right}
   \8 {\7 :left
       \5 :down
       \9 :right}
   \9 {\8 :left
       \6 :down}})

(defn- shortest-paths [pad]
  (->> (for [start (keys pad)
             end (keys pad)
             :when (not= start end)]
         {[start end]
          (let [paths (bfs-shortest-paths
                       (update-vals pad #(set (keys %)))
                       start end)
                fewest-directional-changes (apply min
                                                  (map (fn [path]
                                                         (->> path
                                                              (partition 2 1)
                                                              (map #(get-in pad %))
                                                              (partition-by identity)
                                                              count))
                                                       paths))]
            (into #{}
                  ;; drop paths with lots of directional changes since these will slow down the robots
                  (remove (fn [path]
                            (< fewest-directional-changes
                               (->> path
                                    (partition 2 1)
                                    (map #(get-in pad %))
                                    (partition-by identity)
                                    count))))
                  paths))})
       (into {})))

(def d-pad
  {\A      {:up    :left
            :right :down}
   :up    {\A     :right
           :down :down}
   :left  {:down :right}
   :down  {:up    :up
           :left  :left
           :right :right}
   :right {\A     :up
           :down :left}})

(defn get-moves [pad pad-paths buttons]
  (loop [moves []
         curr \A
         [next & remaining] buttons]
    (cond
      (nil? next)
      moves

      (= curr next)
      (recur (conj moves \A) next remaining)

      :else
      (let [path (get pad-paths [curr next])
            new-moves (->> path
                           (partition 2 1)
                           (mapv #(get-in pad %)))]
        (recur (into moves (conj new-moves \A))
               next
               remaining)))))

(time
 (let [input (str/split-lines (slurp "src/day21/input.txt"))
       shortest-number-paths (shortest-paths number-pad)
       shortest-d-paths (shortest-paths d-pad)]
   (->>
    (for [_ (range 100)
          ;; find the dynamics of shortest paths statistically
          :let [number-startend->path (update-vals shortest-number-paths #(rand-nth (seq %)))
                d-startend->path (update-vals shortest-d-paths #(rand-nth (seq %)))]]
      {:sum (->> input
                 (map (comp
                       count
                       (partial get-moves d-pad d-startend->path)
                       (partial get-moves d-pad d-startend->path)
                       (partial get-moves number-pad number-startend->path)))
                 (map (fn [s n]
                        (let [m (Integer/new (re-find #"\d+" s))]
                          (* n m)))
                      input)
                 (apply +))
       ;; include these to get the hardcoded dynamics of the optimal paths between buttons.
       :number-startend->path number-startend->path
       :d-startend->path d-startend->path})
    (apply min-key :sum))))

;; solution part 2

;; Came from part 1
(def shortest-number-paths
  {[\1 \0] [\1 \2 \0]
   [\9 \7] [\9 \8 \7]
   [\9 \8] [\9 \8]
   [\7 \2] [\7 \4 \1 \2]
   [\2 \0] [\2 \0]
   [\7 \0] [\7 \8 \5 \2 \0]
   [\1 \7] [\1 \4 \7]
   [\2 \5] [\2 \5]
   [\3 \A] [\3 \A]
   [\7 \1] [\7 \4 \1]
   [\3 \6] [\3 \6]
   [\1 \3] [\1 \2 \3]
   [\7 \5] [\7 \8 \5]
   [\4 \1] [\4 \1]
   [\2 \9] [\2 \3 \6 \9]
   [\2 \3] [\2 \3]
   [\A \8] [\A \0 \2 \5 \8]
   [\8 \6] [\8 \9 \6]
   [\7 \4] [\7 \4]
   [\A \4] [\A \3 \6 \5 \4]
   [\4 \9] [\4 \5 \6 \9]
   [\6 \0] [\6 \3 \A \0]
   [\2 \7] [\2 \5 \8 \7]
   [\5 \7] [\5 \8 \7]
   [\5 \9] [\5 \6 \9]
   [\0 \9] [\0 \A \3 \6 \9]
   [\9 \1] [\9 \8 \7 \4 \1]
   [\0 \5] [\0 \2 \5]
   [\7 \8] [\7 \8]
   [\6 \2] [\6 \5 \2]
   [\0 \3] [\0 \A \3]
   [\3 \0] [\3 \2 \0]
   [\9 \6] [\9 \6]
   [\3 \2] [\3 \2]
   [\4 \6] [\4 \5 \6]
   [\0 \6] [\0 \2 \5 \6]
   [\A \6] [\A \3 \6]
   [\8 \A] [\8 \5 \2 \0 \A]
   [\5 \2] [\5 \2]
   [\A \3] [\A \3]
   [\4 \5] [\4 \5]
   [\A \0] [\A \0]
   [\3 \4] [\3 \6 \5 \4]
   [\8 \1] [\8 \5 \2 \1]
   [\3 \9] [\3 \6 \9]
   [\5 \3] [\5 \6 \3]
   [\6 \8] [\6 \5 \8]
   [\8 \4] [\8 \7 \4]
   [\0 \8] [\0 \2 \5 \8]
   [\5 \6] [\5 \6]
   [\2 \6] [\2 \5 \6]
   [\1 \6] [\1 \4 \5 \6]
   [\3 \8] [\3 \2 \5 \8]
   [\3 \1] [\3 \2 \1]
   [\1 \2] [\1 \2]
   [\A \1] [\A \3 \2 \1]
   [\5 \0] [\5 \2 \0]
   [\8 \0] [\8 \5 \2 \0]
   [\5 \4] [\5 \4]
   [\8 \3] [\8 \5 \2 \3]
   [\6 \3] [\6 \3]
   [\7 \9] [\7 \8 \9]
   [\6 \7] [\6 \9 \8 \7]
   [\1 \9] [\1 \2 \3 \6 \9]
   [\9 \2] [\9 \6 \3 \2]
   [\6 \9] [\6 \9]
   [\8 \2] [\8 \5 \2]
   [\9 \A] [\9 \6 \3 \A]
   [\6 \5] [\6 \5]
   [\0 \1] [\0 \2 \1]
   [\2 \A] [\2 \0 \A]
   [\3 \7] [\3 \6 \9 \8 \7]
   [\6 \1] [\6 \3 \2 \1]
   [\1 \8] [\1 \2 \5 \8]
   [\9 \5] [\9 \8 \5]
   [\7 \6] [\7 \8 \9 \6]
   [\4 \7] [\4 \7]
   [\5 \8] [\5 \8]
   [\8 \5] [\8 \5]
   [\4 \2] [\4 \1 \2]
   [\5 \A] [\5 \6 \3 \A]
   [\8 \9] [\8 \9]
   [\2 \4] [\2 \5 \4]
   [\0 \7] [\0 \2 \5 \8 \7]
   [\5 \1] [\5 \2 \1]
   [\0 \2] [\0 \2]
   [\2 \8] [\2 \5 \8]
   [\8 \7] [\8 \7]
   [\6 \A] [\6 \3 \A]
   [\6 \4] [\6 \5 \4]
   [\1 \4] [\1 \4]
   [\7 \A] [\7 \8 \9 \6 \3 \A]
   [\9 \3] [\9 \6 \3]
   [\9 \0] [\9 \6 \3 \A \0]
   [\7 \3] [\7 \8 \9 \6 \3]
   [\A \9] [\A \3 \6 \9]
   [\4 \A] [\4 \5 \6 \3 \A]
   [\1 \A] [\1 \2 \3 \A]
   [\A \2] [\A \3 \2]
   [\1 \5] [\1 \4 \5]
   [\4 \0] [\4 \5 \2 \0]
   [\A \7] [\A \3 \6 \9 \8 \7]
   [\A \5] [\A \0 \2 \5]
   [\9 \4] [\9 \6 \5 \4]
   [\4 \8] [\4 \7 \8]
   [\3 \5] [\3 \2 \5]
   [\2 \1] [\2 \1]
   [\0 \4] [\0 \2 \5 \4]
   [\0 \A] [\0 \A]
   [\4 \3] [\4 \1 \2 \3]})

;; Came from part 1
(def d-startend->path
  {[:left :up]    [:left :down :up]
   [:left :right] [:left :down :right]
   [:right :down] [:right :down]
   [:left \A]    [:left :down :right \A]
   [\A :up]      [\A :up]
   [:right :up]   [:right \A :up]
   [:up :down]    [:up :down]
   [:right \A]   [:right \A]
   [:up :right]   [:up :down :right]
   [\A :right]   [\A :right]
   [\A :left]    [\A :right :down :left]
   [:down \A]    [:down :right \A]
   [:right :left] [:right :down :left]
   [\A :down]    [\A :up :down]
   [:up :left]    [:up :down :left]
   [:down :right] [:down :right]
   [:up \A]      [:up \A]
   [:down :left]  [:down :left]
   [:down :up]    [:down :up]
   [:left :down]  [:left :down]})

(def startend->moves
  (memoize
   (fn [startend]
                 (let [path (get d-startend->path startend)]
                   (conj (->> path
                              (partition 2 1)
                              (mapv #(get-in d-pad %)))
                         \A)))))

(defn- get-moves* [buttons]
  (->> (cons \A buttons)
       (partition 2 1)
       (mapcat startend->moves)))

;; Way too slow. We can't keep track of the full sequence of moves.
;; We need to be able to understand how much each additional layer adds to the moveset
;; Once again a great solution from Maravedis
;; https://clojurians.slack.com/archives/C0GLTDB2T/p1734786379647479?thread_ts=1734786318.464229&cid=C0GLTDB2T
#_
(time
 (let [input (str/split-lines (slurp "src/day21/input.txt"))]
   (->> input
        (map (partial get-moves number-pad shortest-number-paths))
        (map #(nth (iterate get-moves* %) 25))
        (map count)
        (map (fn [s n]
               (let [m (Integer/new (re-find #"\d+" s))]
                 (* n m)))
             input)
        (apply +))))
