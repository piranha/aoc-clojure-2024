(ns day11)

(set! *warn-on-reflection* true)

(defn -next-stones [stone]
  (if (zero? stone)
    [1]
    (let [s      (str stone)
          digits (count s)]
      (if (even? digits)
        (let [n (/ digits 2)]
          [(parse-long (subs s 0 n))
           (parse-long (subs s n))])
        [(* stone 2024)]))))

(defn next-stones [stones]
  (mapcat -next-stones stones))

(defn eleven [n stones]
  (->> (iterate next-stones stones)
       (drop n)
       first
       count))

;;; recursive approach

(def next-stones-rec
  (memoize
    (fn [step stone]
      (let [nstep  (dec step)
            digits (count (str stone))]
        (cond
          (zero? step)   1
          (zero? stone)  (next-stones-rec nstep 1)
          (even? digits) (let [n (/ digits 2)
                               s (str stone)]
                           (+ (next-stones-rec nstep (parse-long (subs s 0 n)))
                              (next-stones-rec nstep (parse-long (subs s n)))))
          :else          (next-stones-rec nstep (* stone 2024)))))))

(defn eleven2 [steps stones]
  (reduce + (map #(next-stones-rec steps %) stones)))


;;; Chris

(defn grouping [xs n]
  (transduce (map val) + 0
    (reduce
      (fn [counts _]
        ;;#p counts
        (apply merge-with +
          (for [[n c] counts]
            (let [xs (-next-stones n)
                  f  (frequencies xs)]
              (update-vals f #(* c %))))))
      (zipmap xs (repeat 1))
      (range n))))

(comment
  (let [counts {125 1 11 1 17 1}]
    (apply merge-with +
      (for [[n c] counts]
        (let [xs (-next-stones n)
              f  (frequencies xs)]
          (update-vals f #(* c %))))))

  (def example (->> (slurp "example-11") (re-seq #"\d+") (mapv parse-long)))
  (def input (->> (slurp "input-11") (re-seq #"\d+") (mapv parse-long)))

  (->> (iterate next-stones example) (drop 24) (take 1) first (take 10))
  (next-stones-rec 75 125)

  (def stones (atom []))
  (reduce + (map #(next-stones-rec 25 (biginteger %)) example))


  (eleven 25 example) ;; => 55312
  (eleven 25 input) ;; => 233050
  (time (eleven 25 input)) ;; => 233050
  (eleven2 25 example) ;; => 55312
  (time (eleven2 25 input)) ;; => 233050
  (time (eleven2 75 input)) ;; => 276661131175807
  )
