(ns day15
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn v+ [[y x] di dj]
  [(+ y di) (+ x dj)])

(defn parse [fname]
  (let [s            (slurp fname)
        [karta path] (.split s "\n\n")
        karta        (into {}
                       (for [[y line] (map-indexed vector (str/split-lines karta))
                             [x c]    (map-indexed vector line)
                             :when    (not= c \.)]
                         (if (= c \@)
                           [:robot [y x]]
                           [[y x] c])))
        path         (.replace path "\n" "")]
    {:robot (:robot karta)
     :karta (dissoc karta :robot)
     :path  path}))

(defn calc-pos [pos m]
  (case m
    \< (v+ pos  0 -1)
    \> (v+ pos  0  1)
    \^ (v+ pos -1  0)
    \v (v+ pos  1  0)))

(defn end-of-line [karta pos m]
  (assert (map? karta))
  (assert (vector? (key (first karta))))

  (loop [pos (calc-pos pos m)]
    (let [v (get karta pos)]
      (case v
        \#  nil
        nil pos
        (recur (calc-pos pos m))))))

(defn one-tick [{:keys [robot karta] :as ctx} m]
  (let [next-pos (calc-pos robot m)]
    (cond
      (not (contains? karta next-pos)) (assoc ctx :robot next-pos)
      (= \# (get karta next-pos))      ctx

      (= \O (get karta next-pos))
      (if-let [end-pos (end-of-line karta next-pos m)]
        (-> ctx
            (assoc :robot next-pos)
            (update :karta set/rename-keys {next-pos end-pos}))
        ctx))))

(defn walk-path [{:keys [path] :as ctx}]
  (reduce one-tick ctx path))

(defn calc-gps [karta]
  (reduce +
    (for [[[y x] c] karta
          :when     (= c \O)]
      (+ (* y 100) x))))

(defn fifteen [fname]
  (-> (parse fname)
      walk-path
      :karta
      calc-gps))

(comment
  (def q (parse "example-15"))

  (end-of-line (:karta q) [3 3] \<)

  (fifteen "example-15-small") ;; => 2028
  (fifteen "example-15") ;; => 10092
  (fifteen "input-15") ;; => 1429911
  )


;;; second part

(defn parse2 [fname]
  (let [s            (slurp fname)
        [karta path] (.split s "\n\n")
        karta        (into {}
                       (for [[y line] (map-indexed vector (str/split-lines karta))
                             [x c]    (map-indexed vector line)
                             :when    (not= c \.)
                             :let     [x1 (* x 2)
                                       x2 (inc x1)]]
                         (cond
                           (= c \@) {:robot [y x1]}
                           (= c \#) {[y x1] c [y x2] c}
                           (= c \O) {[y x1] \] [y x2] \[})))
        path         (.replace path "\n" "")]
    {:robot (:robot karta)
     :karta (dissoc karta :robot)
     :path  path}))

(defn collect-boxes [karta pos1 m] ; pos is a first box position
  (assert (map? karta))
  (assert (vector? (key (first karta))))

  (let [v1   (get karta pos1)
        pos2 (when (#{\[ \]} v1)
               (v+ pos1 0 (case v1 \] -1 \[ 1)))
        v2   (get karta pos2)]
    (cond
      (or (= \# v1)
          (= \# v2))             nil ;; it's stuck
      (and (nil? v1)
           (nil? v2))            :suka-vse-huita
      (and ({\< \>} m) (nil? v1)) pos2)))

(comment
  (def q (parse2 "example-15")))
