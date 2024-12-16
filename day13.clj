(ns day13
  (:require [sane-math.core :as sane]))

(set! *warn-on-reflection* true)

(defn numv [l]
  (mapv parse-long l))

(defn v+ [[i j] [di dj]]
  [(+ i di) (+ j dj)])

(defn v- [[i j] [di dj]]
  [(- i di) (- j dj)])

(defn parse-one [^String s]
  (let [[a b prize] (.split s "\n")]
    {:a     (numv (re-seq #"\d+" a))
     :b     (numv (re-seq #"\d+" b))
     :prize (numv (re-seq #"\d+" prize))}))

(defn parse [^String s]
  (->> (.split s "\n\n")
       (mapv parse-one)))

(defn tolerant-min [x y]
  (cond
    (nil? x) y
    (nil? y) x
    :else    (min x y)))

(defn total-min [x y]
  (cond
    (nil? x) y
    (nil? y) x
    :else    (if (< (+ (:a x) (:b x)) (+ (:a y) (:b y)))
               x
               y)))

(def -make-sol
  (memoize
    (fn [prize a b]
      (cond
        (every? zero? prize) {:a 0 :b 0}
        (some neg? prize)    nil
        :else
        (total-min
          (some-> (-make-sol (v- prize a) a b) (update :a + 3))
          (some-> (-make-sol (v- prize b) a b) (update :b inc)))))))

(defn make-sol [{:keys [a b prize]}]
  (let [res (-make-sol prize a b)]
    (when-not (or (< 300 (:a res 0)) (< 100 (:b res 0)))
      res)))

(defn thirteen [fname]
  (let [tasks (parse (slurp fname))]
    (->> (map make-sol tasks)
         (remove nil?)
         (map #(+ (:a %) (:b %)))
         (reduce +))))

(def x (fn [y] (+ y 20)))

(comment
  (x 30)
  (thirteen "example-13") ;; => 480
  (thirteen "input-13") ;; => 29388
  (def q (parse (slurp
                  "example-13")))

  (inc (inc 1) 2 3)

  (map make-sol q))


(let [x 1]
  x)

;;; math solution

;;; axA + bxB = x
;;; ayA + byB = y
;;; A = (x - bxB) / ax
;;; ay((x - bxB) / ax) + byB = y
;;; ay*x - ay*bxB + ax*byB = y*ax
;;; B = (y*ax - x*ay) / (ax*by - ay*bx)
;;; A = (x - bxB) / ax

(defn solve [{[ax ay] :a [bx by] :b [x y] :prize :as _system}]
  (let [B (sane/math (y * ax - x * ay) / (ax * by - ay * bx))
        A (sane/math (x - bx * B) / ax)]
    (when (and (int? B) (int? A))
      [A B])))

(defn thirteen-math [fname]
  (transduce (comp
               (map solve)
               (remove nil?)
               (map (fn [[A B]] (sane/math 3 * A + B))))
    + 0 (parse (slurp fname))))

(defn thirteen-math2 [fname]
  (transduce (comp
               (map #(update % :prize v+ [10000000000000 10000000000000]))
               (map solve)
               (remove nil?)
               (map (fn [[A B]] (sane/math 3 * A + B))))
    + 0 (parse (slurp fname))))

(comment
  (def q (parse (slurp
                  "example-13")))
  (map solve q)
  (solve (first q))

  (thirteen-math "example-13") ;; => 480
  (thirteen-math "input-13") ;; => 29388
  (thirteen-math2 "example-13") ;; => 875318608908
  (thirteen-math2 "input-13") ;; => 99548032866004
  )
