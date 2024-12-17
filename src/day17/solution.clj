(ns day17.solution
  (:require
   [clojure.math :as math]
   [clojure.string :as str]))

;; solution part 1

(defn combo-operand [{:keys [A B C operand]}]
  (case operand
    4 A
    5 B
    6 C
    7 nil ;; Combo operand 7 is reserved and will not appear in valid programs.
    operand))

(defmulti run :opcode)

(defn- adv [{^long A :A :as params}]
  (quot A (long (math/pow 2 (combo-operand params)))))

;; The adv instruction (opcode 0) performs division. 
;; The numerator is the value in the A register. 
;; The denominator is found by raising 2 to the power of the instruction's combo operand. 
;; (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) 
;; The result of the division operation is truncated to an integer and then written to the A register.
(defmethod run 0 [{:keys [A B C operand] :as params}]
  {:A (adv params)})

;; The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand,
;; then stores the result in register B.
(defmethod run 1 [{:keys [A B C operand] :as params}]
  {:B (bit-xor B operand)})

;; The bst instruction (opcode 2) calculates the value of its combo operand modulo 8
;; (thereby keeping only its lowest 3 bits), then writes that value to the B register.
(defmethod run 2 [{:keys [A B C operand] :as params}]
  {:B (mod (combo-operand params) 8)})

;; The jnz instruction (opcode 3) does nothing if the A register is 0.
;; However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand;
;; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
(defmethod run 3 [{:keys [A B C operand] :as params}]
  (if (zero? A)
    {}
    {:next-instruction-pointer operand}))

;; The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C,
;; then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
(defmethod run 4 [{:keys [A B C operand] :as params}]
  {:B (bit-xor B C)})

;; The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value.
;; (If a program outputs multiple values, they are separated by commas.)
(defmethod run 5 [{:keys [A B C operand output] :as params}]
  {:output (str output
                (if (empty? output) "" ",")
                (mod (combo-operand params) 8))})

;; The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register.
;; (The numerator is still read from the A register.)
(defmethod run 6 [{:keys [A B C operand] :as params}]
  {:B (adv params)})

;; The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register.
;; (The numerator is still read from the A register.)
(defmethod run 7 [{:keys [A B C operand] :as params}]
  {:C (adv params)})

(time
 (let [[[A B C] program] (map (comp vec #(map Integer/new %) #(re-seq #"\d+" %))
                              (str/split (slurp "src/day17/input.txt") #"\n\n"))]
   (loop [A A
          B B
          C C
          current-instruction-pointer 0
          output ""]
     (let [opcode (get program current-instruction-pointer)
           operand (get program (inc current-instruction-pointer))]
       (if (nil? opcode) ;; halt
         output
         (let [params {:opcode opcode
                       :operand operand
                       :A A
                       :B B
                       :C C
                       :output output}
               {:keys [A B C output next-instruction-pointer]} (merge params (run params))]
           (recur A B C (or next-instruction-pointer (+ current-instruction-pointer 2)) output)))))))

;; solution part 2

(defn- condensed
  "Equivalent to my program input 2,4,1,1,7,5,1,5,4,1,5,5,0,3,3,0"
  [A]
  (loop [A A
         out []]
    (if (zero? A)
      out
      (recur (bit-shift-right A 3)
             (conj out
                   (bit-and (bit-xor (bit-xor (bit-xor (bit-and A 7) 1) 5)
                                     (bit-shift-right A (bit-xor (bit-and A 7) 1)))
                            7))))))

;; solution from Maravedis
(time
 (let [[_ program] (map (comp vec #(map Integer/new %) #(re-seq #"\d+" %))
                        (str/split (slurp "src/day17/input.txt") #"\n\n"))
       rprogram (reverse program)]
   (loop [power 15
          valid #{0xFFFFFFFFFFFF}] ;; max possible value for A which produces a 16 number long program
     (if (< power 0)
       (apply min valid) ;; take the smallest valid value
       (recur (dec power)
              (mapcat (fn [o] (for [A     (take 8 (iterate #(- % (long (bit-shift-left 1 (* 3 power)))) o))
                                    :let  [output (reverse (condensed A))
                                           n-digits (- 16 power)]
                                    :when (= (take n-digits output)
                                             (take n-digits rprogram))]
                                A))
                      valid))))))
