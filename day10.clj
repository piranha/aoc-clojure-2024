(ns day10
  (:require [clojure.java.io :as io]))

(defn char->digit [c]
  (- (int c) 48))

(defn parse [fname]
  (let [karta (vec (for [line (line-seq (io/reader fname))]
                     (mapv char->digit line)))]
    {:karta karta
     :heads (for [[i line] (map-indexed vector karta)
                  [j n]    (map-indexed vector line)
                  :when    (zero? n)]
              [i j])}))

(defn v+ [[i j] [di dj]]
  [(+ i di) (+ j dj)])

(defn make-dirs [pos]
  (mapv #(v+ pos %) [[0  1]
                     [1  0]
                     [-1 0]
                     [0 -1]]))

(defn in-bounds? [size [i j]]
  (and (< -1 i size)
       (< -1 j size)))

(defn headhunter [start karta]
  (let [value (get-in karta start)]
    (if (= value 9)
      #{start}
      (set
        (for [dir   (make-dirs start)
              :when (and (in-bounds? (count karta) dir)
                         (= (inc value) (get-in karta dir)))
              head  (headhunter dir karta)]
          head)))))

(defn trails [start karta]
  (let [value (get-in karta start)]
    (if (= value 9)
      [[start]]
      (for [dir   (make-dirs start)
            :when (and (in-bounds? (count karta) dir)
                       (= (inc value) (get-in karta dir)))
            trail  (trails dir karta)]
        (cons start trail)))))

(defn ten [fname]
  (let [{:keys [heads karta]} (parse fname)]
    (->> (map #(count (headhunter % karta)) heads)
         (reduce +))))

(defn ten2 [fname]
  (let [{:keys [heads karta]} (parse fname)]
    (->> (map #(count (trails % karta)) heads)
         (reduce +))))


(comment
  (def fname "example-10")
  (def q (parse fname))
  (map #(count (headhunter % (:karta q))) (:heads q))
  (get-in (:karta q) [0 2])
  (ten "example-10")
  (ten "input-10") ;; => 607
  (ten2 "input-10") ;; => 1384
  )
