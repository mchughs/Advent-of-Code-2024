(ns day24.solution
  (:require
   [clojure.data :as data]
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [clojure.string :as str]))

;; solution part 1

(def s->op
  {"AND" bit-and
   "XOR" bit-xor
   "OR" bit-or})

(defn- binary->decimal [n]
  (Long/parseLong n 2))

(time
 (let [[wire-input _ gate-input] (partition-by empty? (str/split-lines (slurp "src/day24/input.txt")))
       init-wires (into {}
                        (map #(let [[_ wire s] (re-find #"(\w{3})\: (\d+)" %)]
                                {wire (Integer/new s)}))
                        wire-input)
       gates (map #(let [[_ w1 s w2 w3] (re-find #"(\w{3}) (\w+) (\w{3}) -> (\w{3})" %)
                         op (s->op s)]
                     {:w1 w1 :op op :w2 w2 :w3 w3})
                  gate-input)
       z-wires (into #{}
                     (keep (fn [{:keys [w3]}]
                             (when (str/starts-with? w3 "z")
                               w3))
                           gates))
       gate-fns (map (fn [{:keys [w1 op w2 w3]}]
                       (fn [wires]
                         (if (and (get wires w1) (get wires w2))
                           (assoc wires w3 (op (get wires w1)
                                               (get wires w2)))
                           wires)))
                     gates)]
   (->> init-wires
        (iterate #(reduce (fn [wires f] (f wires)) % gate-fns))
        (drop-while #(seq (set/difference z-wires (set (keys %)))))
        first
        (filter (fn [[k _]] (contains? z-wires k)))
        (sort-by first)
        (map last)
        reverse
        str/join
        binary->decimal)))

;; solution part 2

(defn- wires->n [wire-set wires]
  (->> wires
       (filter (fn [[k _]] (contains? wire-set k)))
       (sort-by first)
       (map last)
       reverse
       str/join
       binary->decimal))

(defn- build-tree [wire-set init-values gates]
  #_(let [wires (keys gates)]
    (letfn [(expand
             ([loop-wire wire]
              (if (= loop-wire wire)
                nil
                (if-let [{:keys [w1 op w2]} (get gates wire)]
                  (let [w1' (expand loop-wire w1)
                        w2' (expand loop-wire w2)]
                    (op w1' w2'))
                  (get init-values wire)))))
            (first-expansion [wire]
              (let [{:keys [w1 op w2]} (get gates wire)]
                (op (expand wire w1) (expand wire w2))))]
      (->> wires
           (map (fn [wire] {wire (first-expansion wire)}))
           (into {})
           (wires->n wire-set))))
  
  (let [wires (keys gates)]
    (letfn [(expand [seen wire]
                    (prn seen)
                    (when-not (contains? seen wire) ;; looping
                      (if-let [{:keys [w1 op w2]} (get gates wire)]
                        (let [w1' (expand (conj seen w1 w2) w1)
                              w2' (expand (conj seen w1 w2) w2)]
                          (when (and w1' w2') ;; no loop deeper in
                            (op w1' w2')))
                        (get init-values wire))))]
      (let [tree (map (fn [wire] {wire (expand #{} wire)}) wires)]
        (when-not (some (comp nil? last) tree) ;; loop detected
          tree
          #_(->> tree
               (into {})
               (wires->n wire-set)))))))

#_
(defn valid? [tree digit]
  (= bit-xor (get tree :op)))org.clojure/core.match {:mvn/version "1.1.0"}

#_(time
   (let [[wire-input _ gate-input] (partition-by empty? (str/split-lines (slurp "src/day24/input.txt")))
         init-wires (into {}
                          (map #(let [[_ wire s] (re-find #"(\w{3})\: (\d+)" %)]
                                  {wire (Integer/new s)}))
                          wire-input)
         gates (into {}
                     (map #(let [[_ w1 s w2 w3] (re-find #"(\w{3}) (\w+) (\w{3}) -> (\w{3})" %)
                                 op (s->op s)]
                             [w3 {:w1 w1 :op op :w2 w2}]))
                     gate-input)
         x-wires (into #{}
                       (keep #(when (str/starts-with? % "x")
                                %))
                       (keys init-wires))
         y-wires (into #{}
                       (keep #(when (str/starts-with? % "y")
                                %))
                       (keys init-wires))
         z-wires (into #{}
                       (keep #(when (str/starts-with? % "z")
                                %))
                       (keys gates))
         xyz-wires (set/union x-wires y-wires z-wires)
         x (wires->n x-wires init-wires)
         y (wires->n y-wires init-wires)
         target (bit-and x y)
         inlined-gates (->> gates
                            build-tree
                            (filter (fn [[k _]] (contains? z-wires k)))
                            (into {}))

         #_#_calc-z (fn [gates]
                      (let [gate-fns (map (fn [{:keys [w1 op w2 w3]}]
                                            (fn [wires]
                                              (if (and (get wires w1) (get wires w2))
                                                (assoc wires w3 (op (get wires w1)
                                                                    (get wires w2)))
                                                wires)))
                                          gates)]
                        (->> init-wires                      (do (prn :looped))

                             (iterate #(reduce (fn [wires f] (f wires)) % gate-fns))
                             (drop-while #(seq (set/difference z-wires (set (keys %)))))
                             first
                             (wires->n z-wires))))
         #_#_inline-ops (fn [gates]
                          (let [m (reduce
                                   (fn [acc {:keys [w1 op w2 w3]}]
                                     (assoc acc w3 {:op op :w1 w1 :w2 w2}))
                                   {}
                                   gates)]
                            (reduce
                             (fn [acc {:keys [w1 op w2 w3]}]
                               (conj acc
                                     {:op op
                                      :w1 (get m w1 w1)
                                      :w2 (get m w2 w2)
                                      :w3 w3}))
                             []
                             gates)))
         #_#_swap-four-pairs (fn [gates]
                         ;; extra counting some pairs since the pair [a b] is the same as [b a]
                               (for [eight (combo/permuted-combinations (map :w3 gates) 4)
                                     :let [pairs (partition 2 eight)
                                           pair-map (reduce (fn [m [a b]] (-> m (assoc a b) (assoc b a))) {} pairs)]]
                                 {:pairs pairs
                                  :swapped-gates
                                  (reduce (fn [acc {:keys [w3] :as gate}]
                                            (conj acc
                                                  (if-let [w3' (get pair-map w3)]
                                                    (assoc gate :w3 w3')
                                                    gate)))
                                          []
                                          gates)}))]

     (->> inlined-gates
          (keep (fn [[k v]] (when-not (valid? v k) k))))
  ;;  (update-vals inlined-gates (comp #(dec (quot % 2)) count extract-variables))

     #_(->> gates
            (iterate inline-ops)
            (take 3)
            last
            #_#_#_(drop-while #(let [wires (into #{}
                                                 (mapcat (fn [{:keys [w1 w2 w3]}] [w1 w2 w3]))
                                                 %)]
                                 (seq (set/difference wires xyz-wires))))
                first
              clojure.pprint/pprint)

     #_(first
        (for [{:keys [pairs swapped-gates]} (swap-four-pairs gates)
              :let [z (calc-z swapped-gates)]
              :when (= target z)]
          pairs))))


; --- 

(defn- swap-pairs [gates pairs]
  (reduce (fn [acc [a b]]
            (-> acc
                (assoc a (get acc b))
                (assoc b (get acc a))))
          gates
          pairs))

(defn- cartesian-product-one-to-one
  "Takes two sets and returns all the possible one-to-one pairings between them."
  [set1 set2]
  (let [permutations (map #(map vector set1 %) (combo/permutations (vec set2)))]
    permutations))

(time
 (let [[wire-input _ gate-input] (partition-by empty? (str/split-lines (slurp "src/day24/input.txt")))
       init-wires (into {}
                        (map #(let [[_ wire s] (re-find #"(\w{3})\: (\d+)" %)]
                                {wire (Integer/new s)}))
                        wire-input)
      ;;  gates (mapv #(let [[_ w1 s w2 w3] (re-find #"(\w{3}) (\w+) (\w{3}) -> (\w{3})" %)
      ;;                     op (s->op s)]
      ;;                 {:w1 w1 :op op :w2 w2 :w3 w3})
      ;;              gate-input)
       gates (into {}
                   (map #(let [[_ w1 s w2 w3] (re-find #"(\w{3}) (\w+) (\w{3}) -> (\w{3})" %)
                               op (s->op s)]
                           [w3 {:w1 w1 :op op :w2 w2}]))
                   gate-input)
       x-wires (into #{}
                     (keep #(when (str/starts-with? % "x")
                              %))
                     (keys init-wires))
       y-wires (into #{}
                     (keep #(when (str/starts-with? % "y")
                              %))
                     (keys init-wires))
       z-wires (into #{}
                     (keep #(when (str/starts-with? % "z") %))
                     #_(keep #(when (str/starts-with? (:w3 %) "z")
                                (:w3 %)))
                     (keys gates)
                    ;;  gates
                     )
       xyz-wires (set/union x-wires y-wires z-wires)
       x (wires->n x-wires init-wires)
       y (wires->n y-wires init-wires)
       target (bit-and x y)
       calc-z (fn [gates] (build-tree z-wires init-wires gates))
       #_(fn [gates]
           (let [gate-fns
                 (map (fn [{:keys [w1 op w2 w3]}]
                        (fn [wires]
                          wire-set                (if (and (get wires w1) (get wires w2))
                                                    (assoc wires w3 (op (get wires w1)
                                                                        (get wires w2)))
                                                    wires)))
                      gates)
                 wires
                 (->> init-wires
                      (iterate #(reduce (fn [wires f] (f wires)) % gate-fns))
                           ;; maximum cycles
                      (take (* 2 (inc (count z-wires))))
                      (drop-while #(seq (set/difference z-wires (set (keys %)))))
                      first)]
             (if wires
               (wires->n z-wires wires)
               0)))
       known-swaps #{"z27" "msp" "mps" "vhm" "z14" "z39"}
       ;; found the candidates with this code, and then figured out the pairings by hand 
       #_(->> gates
              (filter (fn [{:keys [w1 op w2 w3]}]
                         ;; Some analysis of the circuit according to a https://en.wikipedia.org/wiki/Adder_(electronics)#Ripple-carry_adder
                        (or (and (= bit-xor op)
                                 (every? #(not (contains? xyz-wires %)) [w1 w2 w3]))
                            (and (contains? z-wires w3)
                                 (not= bit-xor op)
                                 (not= "z45" w3)))))
              (map :w3)
              (into #{}))
       possible-pairs (cartesian-product-one-to-one #{"z27" "z14" "z39"} #{"msp" "mps" "vhm"})] 
   (first
    (for [pairs possible-pairs
          :let [gates' (swap-pairs gates pairs)
                z (calc-z gates')]]
      z))
   #_(first
      (drop-while zero?
                  (for [pairs possible-pairs
                        last-pair (combo/combinations (set/difference (into #{} (map first) gates)
                                                                      known-swaps)
                                                      2)
                        :let [gates' (swap-pairs gates (conj pairs last-pair))
                              z (calc-z gates')]]
                    z)))
   #_(take 1000
           (for [pairs (combo/cartesian-product #{"z27" "z14" "z39"} #{"msp" "mps" "vhm"})
                 pair (combo/combinations (set/difference (into #{} (map first) gates)
                                                  ;;  (set (flatten known-swaps))
                                                          known-swaps)
                                          2)
                 :let [gates'
                       (swap-pairs gates (conj pairs pair))
                ;; (swap-pairs partially-swapped [pair])
                       z (calc-z gates')]
          ;; :when (= target z)
                 ]
             z))))

