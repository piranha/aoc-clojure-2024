(ns aoc
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

;;; utils

(defn read-ints [fname]
  (->> (line-seq (io/reader fname))
       (map #(->> (str/split % #"\s+")
                  (mapv parse-long)))))

(defn transpose [cols]
  (apply mapv vector cols))

;;; one

(defn one []
  (let [[arr1 arr2] (transpose (read-ints "input-1"))]
    (->> (map (comp abs -) (sort arr1) (sort arr2))
         (reduce +))))

(defn one-2 []
  (let [[arr1 arr2] (transpose (read-ints "input-1"))
        fqs         (frequencies arr2)]
    (->> (map #(* % (fqs % 0)) arr1)
         (reduce +))))

(comment
  (one) ;; => 1320851
  (one-2) ;; => 26859182
  )

;;; two

(defn safe? [report]
  (let [res (->> (partition 2 1 report)
                 (map #(apply - %)))]
    (or (every? #{1 2 3} res)
        (every? #{-1 -2 -3} res))))

(defn but1 [v i] ; v[:i] + v[i+1:]
  (into (subvec v 0 i) (subvec v (inc i))))

(defn two [fname]
  (->> (for [report (read-ints fname)]
         (safe? report))
       (frequencies)))

(defn two-2 [fname]
  (->> (for [report (read-ints fname)]
         (some safe? (map (partial but1 report) (range (count report)))))
       (frequencies)))

(comment
  (two "input-2") ;; => {false 558, true 442}
  (two-2 "input-2") ;; => {true 489, false 511}
  )

;;; three

(defn three [fname]
  (->> (for [[_ x y] (->> (slurp fname)
                          (re-seq #"mul\((\d+),(\d+)\)"))]
         (* (parse-long x) (parse-long y)))
       (reduce +)))

(defn three-2 [fname]
  (let [mul? (atom true)]
    (->> (for [[m x y] (->> (slurp fname)
                            (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)"))]
           (case m
             "do()"    (do (reset! mul? true) 0)
             "don't()" (do (reset! mul? false) 0)
             (if @mul?
               (* (parse-long x) (parse-long y))
               0)))
         (reduce +))))

(comment
  (three "input-3") ;; => 156388521
  (three-2 "input-3") ;; => 75920122
  )

;;; fourth

(defn rep-recur [n f v]
  (reduce (fn [acc _] (f acc)) v (range n)))

(defn dirs [[i j]]
  (for [[fi fj] [[inc      identity] ; vertical
                 [identity inc]      ; horizontal
                 [inc      inc]      ; diag 1
                 [inc      dec]]]    ; diag 2
    (for [r (range 4)]
      [(rep-recur r fi i)
       (rep-recur r fj j)])))

(def XMAS? #{[\X \M \A \S]
             [\S \A \M \X]})

(defn xmas-cnt [mx ij]
  (when (let [l (get-in mx ij)] (or (= l \X) (= l \S)))
    (->> (for [dir (dirs ij)]
           (if (XMAS? (map #(get-in mx %) dir))
             1
             0))
         (reduce +))))

(defn finder [mx]
  (->> (let [l (count (first mx))]
         (for [i (range l)
               j (range l)]
           (or (xmas-cnt mx [i j]) 0)))
       (reduce +)))


(comment
  (-> (line-seq (io/reader "input-4"))
      vec
      finder) ;; => 2560
  )

;;; fourth - part two

(def MAS? #{[\M \A \S]
            [\S \A \M]})

(defn masmas? [mx [i j]]
  (when (= (get-in mx [i j]) \A)
    (and (MAS? [(get-in mx [(dec i) (dec j)])
                (get-in mx [i j])
                (get-in mx [(inc i) (inc j)])])
         (MAS? [(get-in mx [(inc i) (dec j)])
                (get-in mx [i j])
                (get-in mx [(dec i) (inc j)])]))))

(defn finder2 [mx]
  (->> (let [l (count (first mx))]
         (for [i (range l)
               j (range l)]
           (if (masmas? mx [i j]) 1 0)))
       (reduce +)))

(comment
  (-> (line-seq (io/reader "real-example-4"))
      vec
      (masmas? [1 2]))
  (-> (line-seq (io/reader "input-4"))
      vec
      finder2) ;; => 1910
)


;;; five

(defn parse [fname]
  (let [[order [_ & updates]] (->> (line-seq (io/reader fname))
                                   (split-with not-empty))]
    [(-> (->> (mapv #(str/split % #"\|") order)
              (group-by first))
         (update-vals #(set (mapv second %))))
     (->> updates
          (mapv #(str/split % #",")))]))

(defn correct? [order update]
  (boolean (reduce
             (fn [seen page]
               (if (seq (set/intersection (get order page) seen))
                 (reduced nil)
                 (conj seen page)))
             #{}
             update)))

(defn get-middle [v]
  (get v (-> (count v) (/ 2) int)))

(defn five [fname]
  (let [[order updates] (parse fname)]
    (->> (filter #(correct? order %) updates)
         (map #(parse-long (get-middle %)))
         (reduce +))))

(defn sort-update [order update]
  (sort-by
    identity
    (fn [x y]
      (cond
        (contains? (get order x) y) -1
        (contains? (get order y) x) 1
        :else                       0))
    update))

(defn five-2 [fname]
  (let [[order updates] (parse fname)]
    (->> (remove #(correct? order %) updates)
         (map #(sort-update order %))
         (map #(parse-long (get-middle (vec %))))
         (reduce +))))

(comment
  (def fname "example-5")
  (parse fname)
  (five "input-5")
  (five-2 "input-5"))


;;; six

(def dirs6 {\^ :up
            \> :right
            \v :down
            \< :left})

(defn parse6 [lines]
  (let [res (into {}
              (for [[i line] (map-indexed vector lines)
                    [j c]    (map-indexed vector line)
                    :when    (not= c \.)]
                (if (or (= c \#) (= c \X))
                  {[i j] c}
                  {:pos [i j] :dir (get dirs6 c)})))]
    {:pos (:pos res)
     :dir (:dir res)
     :obstacle (dissoc res :pos :dir)
     :width    (count (first lines))
     :height   (count lines)}))

(defn inside? [h w [i j]]
  (and (< -1 i h)
       (< -1 j w)))

(def turn-right {:up    :right
                 :right :down
                 :down  :left
                 :left  :up})

(defn inc-pos [{:keys [dir pos width height] :as _karta}]
  (let [[i j]    pos
        next-pos (case dir
                   :up    [(dec i) j]
                   :down  [(inc i) j]
                   :left  [i (dec j)]
                   :right [i (inc j)])]
    (when (inside? width height next-pos)
      next-pos)))

(defn next-step [{:keys [known dir pos]
                  :or   {known #{}}
                  :as   karta}]
  (let [known (conj known [dir pos])]
    (when-let [next-pos (inc-pos karta)]
      (cond
        (contains? known [dir next-pos])    (throw (ex-info "loop" {:loop true}))
        (get-in karta [:obstacle next-pos]) (-> karta
                                                (assoc :known known)
                                                (update :dir turn-right))
        :else                               (assoc karta :pos next-pos :known known)))))

(defn make-route [karta]
  (->> (iterate next-step karta)
       (take-while some?)
       (map :pos)
       set))

(defn six [fname]
  (let [karta (parse6 (line-seq (io/reader fname)))
        steps (make-route karta)]
    (count steps)))

(defn obs-locations [karta known]
  (for [[i [dir pos]] (map-indexed vector known)
        :let  [;;_ #p [i pos dir]
               obs-pos (inc-pos (assoc karta :dir dir :pos pos))]
        :when (not (get-in karta [:obstacle obs-pos]))]
    (try
      (make-route (assoc-in karta [:obstacle obs-pos] \#))
      nil
      (catch Exception e
        (when (:loop (ex-data e))
          obs-pos)))))

(defn six2 [fname]
  (let [karta (parse6 (line-seq (io/reader fname)))
        known (->> (iterate next-step karta)
                   (take-while some?)
                   last
                   :known)]
    (->> (obs-locations karta known)
         (remove nil?)
         set
         count)))

(comment
  (def q (parse6 (line-seq (io/reader fname))))

  (make-route (-> (update q :obstacle assoc [24 27] \#)
                  (assoc :dir :right :pos [24 26])))
  (get-in q [:obstacle [0 4]])
  (def fname "input-6")
  (six "input-6") ;; => 5564
  (time (six2 "input-6")) ;; => 1976
  )


;;; seven

(defn combine [x y]
  (let [n (loop [n 0 y y] (if (< y 10) (inc n) (recur (inc n) (quot y 10))))]
    (+ (* x (int (Math/pow 10 n))) y)))

(def OPS [* +])
(def OPS2 [* + combine])

(defn op-permutations [ops n]
  (if (zero? n)
    '(nil)
    (let [rest-perms (op-permutations ops (dec n))]
      (for [op   ops
            perm rest-perms]
        (cons op perm)))))

(defn make-calcs [ops xs]
  (let [n (dec (count xs))]
    (for [perm (op-permutations ops n)]
      (reduce
        (fn [res [op n]]
          (op res n))
        (first xs)
        (map vector perm (rest xs))))))

(defn calc? [ops res xs]
  (some?
    (some #(= res %) (make-calcs ops xs))))

(defn run-seven [ops fname]
  (reduce +
    (for [line  (line-seq (io/reader fname))
          :let  [[res & xs] (->> (re-seq #"\d+" line)
                                 (map parse-long))]
          :when (calc? ops res xs)]
      res)))

(defn seven [fname]
  (run-seven OPS fname))

(defn seven2 [fname]
  (run-seven OPS2 fname))


(comment
  (def fname "input-7")
  (seven fname) ;; => 303766880536
  (seven2 fname) ;; => 337041851384440
  )


;;; eight

(defn parse8 [lines]
  (into {}
    (for [[i line] (map-indexed vector lines)
          [j c]    (map-indexed vector line)
          :when    (not= c \.)]
      [[i j] c])))

(defn pairs [xs]
  (for [x xs
        y xs
        :when (> (compare y x) 0)]
    [x y]))

(defn in-bounds? [size [i j]]
  (and (< -1 i size)
       (< -1 j size)))

(defn v+ [[i j] [di dj]]
  [(+ i di) (+ j dj)])

(defn v- [[i j] [di dj]]
  [(- i di) (- j dj)])

(defn make-antinodes [size [pos1 pos2] multi?]
  (let [diff  (v- pos1 pos2)
        taker (if multi?
                (take-while (partial in-bounds? size))
                (comp (drop 1) (take 1) (filter (partial in-bounds? size))))]
    (concat
      (->> (iterate (fn [pos] (v+ pos diff)) pos1)
           (transduce taker conj []))
      (->> (iterate (fn [pos] (v- pos diff)) pos2)
           (transduce taker conj [])))))

(defn -render-map8 [size karta]
  (str/join "\n"
    (for [i (range size)]
      (str/join ""
        (for [j (range size)]
          (or (get karta [i j])
              "."))))))

(defn render-map8 [size karta antinodes]
  (-render-map8 size #p (into #p karta #p (zipmap antinodes (repeat "#")))))

(defn eight [fname & [multi?]]
  (let [karta     (parse8 (line-seq (io/reader fname)))
        size      (count (first (line-seq (io/reader fname))))
        groups    (-> (group-by second karta)
                      (update-vals #(map first %)))
        antinodes (set (for [[_k group] groups
                             pair       (pairs group)
                             antinode   (make-antinodes size pair multi?)]
                         antinode))]
    (count antinodes)))

(defn eight2 [fname]
  (eight fname true))

(comment
  (def fname "example-8")
  (eight "example-8" true)
  (eight "input-8") ;; => 280
  (eight2 "input-8") ;; => 958
  )
