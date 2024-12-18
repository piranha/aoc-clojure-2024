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
           :source  (str/trim (second (.split program ": ")))
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
      ;;#p [op arg A B C c]
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
       :out))

(defn exec-recur [#_{:keys [pointer program A B C] :as ctx}
                  pointer program A B C]
  (loop [i pointer
         A A
         B B
         C C
         out []]
    (if-let [[op arg] (get program i)]
      (let [c (case arg 4 A 5 B 6 C arg)]
        ;;#p [op arg A B C c]
        (case op
          :adv (recur (inc i) (quot A (pow2 c)) B C out)
          :bdv (recur (inc i) A (quot A (pow2 c)) C out)
          :cdv (recur (inc i) A B (quot A (pow2 c)) out)
          :bxl (recur (inc i) A (bit-xor B arg) C out)
          :bst (recur (inc i) A (mod c 8) C out)
          :bxc (recur (inc i) A (bit-xor B C) C out)
          :out (recur (inc i) A B C (conj out (mod c 8)))
          :jnz (recur (if (zero? A) (inc i) arg) A B C out)))
      out)))

(defn seventeen [fname]
  (->> (parse fname)
       exec
       (str/join ",")))

(defn bruteforce
  ([ctx] (bruteforce ctx 0 1000000))
  ([ctx start end]
   (let [target (map parse-long (.split (:source ctx) ","))
         {:keys [pointer program A B C]} ctx]
     (loop [A start]
       (when (zero? (mod A 1000000))
         (prn A))
       (cond
         (= A end) nil
         (= (exec-recur pointer program A B C) target) A
         :else (recur (inc A)))))))

(defn parallel-bruteforce [ctx max-value threads]
  (let [size    (quot max-value threads)
        ranges  (for [[start end] (partition-all 2 1 (range 0 max-value size))]
                  [start (or end max-value)])
        futures (mapv #(future
                         (bruteforce ctx (first %) (second %)))
                  ranges)]
    (->> futures
         (mapv deref)
         (remove nil?)
         first)))

;;; part 2

(defn step [A]                          ; actual `input-17` instructions
  (let [B (bit-and A 2r111)
        B (bit-xor B 2r101)
        C (bit-shift-right A B)
        B (bit-xor B 2r110)
        B (bit-xor B C)]
    (bit-and B 2r111)))

(defn -finder [out A]
  (if (empty? out)
    A
    (let [x (first out)
          A (bit-shift-left A 3)]
      (apply min Long/MAX_VALUE
        (for [a     (range A (+ A 8))
              :when (= x (step a))]
          (-finder (next out) a))))))

(defn finder [out]
  (-finder (reverse out) 0))

(comment
  (filter #(= 3 (step %)) (range 24 32))

  (bit-shift-left 2r011 3)

  (finder [2,4,1,5,7,5,1,6,4,1,5,5,0,3,3,0])
  ;; => 107413700225434

  (user/time+ (step 107413700225434))
)

(comment
  [[:bst :A] [:bxl 5] [:cdv :B] [:bxl 6] [:bxc nil] [:out :B] [:adv 3] [:jnz 0]]

  (loop [A   859309601803472
         out []]
    (let [B   (bit-and A 2r111)      ; only 3 bits
          B   (bit-xor B 2r101)
          C   (bit-shift-right A B)
          B   (bit-xor B 2r110)
          B   (bit-xor B C)
          out (conj out (bit-and B 2r111))
          A   (bit-shift-right A 3)]
      ;;#p [:======= (last out)]
      (if (zero? A)
        out
        (recur A out))))

  ;; cdv: A // 2^B -> C :: A >> B -> C

  (reverse
    (map #(apply str %)
      (partition-all 3 (str "0" (Integer/toString 60589763 2)))))

  ;; => ("011" "000" "011" "011" "000" "001" "111" "100" "011")
  [2r011 2r101 2r000 2r001 2r101 2r001 2r101 2r001 2r000]
       ;; => [3 5 0 1 5 1 5 1 0]

      ;; => [5 2 4 1 5 7 5 1 6 4 1 5 5 0 3 3 0]
       (count "2,4,1,5,7,5,1,6,4,1,5,5,0,3,3,0")

  (def fname "example-17")
  (def q (parse "example-17"));; => {:pointer 0, :program ([:adv 1] [:out 4] [:jnz 0]), :A 729, :B 0, :C 0}
  (def q (parse "input-17"))
;; => {:pointer 0, :out [], :program [[:bst 4] [:bxl 5] [:cdv 5] [:bxl 6] [:bxc 1] [:out 5] [:adv 3] [:jnz 0]], :A 60589763, :B 0, :C 0}

  (def q (->  q exec-one exec-one))
  (exec (parse "example-17")) ;; => "4,6,3,5,6,3,5,2,1,0"
  (let [p (parse "input-17")]
    (user/time+ (exec p))) ;; => "3,5,0,1,5,1,5,1,0"
  (let [{:keys [pointer program A B C]} (parse "input-17")]
    (user/time+ (exec-recur pointer program A B C))) ;; => "3,5,0,1,5,1,5,1,0"

  (parse-program "0,3,5,4,3,0")

  (exec (assoc (parse "example-17-unknown") :A 117440))
  (user/time+ 2000 (bruteforce (parse "example-17-unknown")))
  (time (bruteforce (parse "input-17")))
  (parallel-bruteforce (parse "input-17") 1000000000 8)
  )
