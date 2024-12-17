(ns day17
  (:require [clojure.string :as str]))

;;; 4 - A, 5 - B, 6 - C

;; 0 :adv combo - 'A // 2^combo' -> A
;; 1 :bxl lit - 'B XOR lit' -> B
;; 2 :bst combo - 'combo MOD 8' -> B
;; 3 :jnz lit - 'if A != 0 then jump lit'
;; 4 :bxc lit - 'XOR B C' -> B
;; 5 :out combo - 'outputs combo'
;; 6 :bdv combo - 'A // 2^combo' -> B
;; 7 :cdv combo - 'A // 2^combo' -> C

(def OPS
  {"0" :adv
   "1" :bxl
   "2" :bst
   "3" :jnz
   "4" :bxc
   "5" :out
   "6" :bdv
   "7" :cdv})

(defn parse-program [s]
  (->> (re-seq #"\d+" s)
       (partition 2)
       (mapv (fn [[op arg]] [(get OPS op) (parse-long arg)]))))

(defn parse [fname]
  (let [[regs program] (.split (slurp fname) "\n\n")]
    (into {:pointer 0
           :out     []
           :program (parse-program program)}
      (->> (re-seq #"([A-C]): (\d+)" regs)
           (map (fn [[_ r v]] [(keyword r) (parse-long v)]))))))

(defn combo [ctx v]
  (case v
    4 (:A ctx)
    5 (:B ctx)
    6 (:C ctx)
    7 (throw (ex-info "Reserved operation" {:v v}))
    v))

(defn nextp [ctx]
  (update ctx :pointer inc))

(defn pow2 [arg]
  (long (Math/pow 2 arg)))

(defn exec-one [{:keys [pointer program A B C] :as ctx}]
  (when-let [[op arg] (get program pointer)]
    (let [c   (combo ctx arg)
          ctx (nextp ctx)]
      (case op
        :adv (assoc ctx :A (quot A (pow2 c)))
        :bdv (assoc ctx :B (quot A (pow2 c)))
        :cdv (assoc ctx :C (quot A (pow2 c)))
        :bxl (assoc ctx :B (bit-xor B arg))
        :bst (assoc ctx :B (mod c 8))
        :bxc (assoc ctx :B (bit-xor B C))
        :out (update ctx :out conj (mod c 8))
        :jnz (cond-> ctx (pos? A) (assoc :pointer arg))))))

(defn exec [ctx]
  (->> (iterate exec-one ctx)
       (take-while some?)
       last
       :out
       (str/join ",")))

(comment
  (def fname "example-17")
  (def q (parse "example-17"));; => {:pointer 0, :program ([:adv 1] [:out 4] [:jnz 0]), :A 729, :B 0, :C 0}
  (def q (parse "input-17"))
;; => {:pointer 0, :out [], :program [[:bst 4] [:bxl 5] [:cdv 5] [:bxl 6] [:bxc 1] [:out 5] [:adv 3] [:jnz 0]], :A 60589763, :B 0, :C 0}

  (def q (->  q exec-one exec-one))
  (exec (parse "example-17")) ;; => "4,6,3,5,6,3,5,2,1,0"
  (exec (parse "input-17")) ;; => "3,5,0,1,5,1,5,1,0"

  (parse-program "0,3,5,4,3,0")
)
