(ns day16
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.test :refer [deftest is]]))

(defn v+ [[y x] di dj]
  [(+ y di) (+ x dj)])

(defn parse [fname]
  (into {}
    (for [[y line] (map-indexed vector (line-seq (io/reader fname)))
          [x c]    (map-indexed vector line)
          :when    (not= c \.)]
      (case c
        \S [:start [y x]]
        \E [:end [y x]]
        \# [[y x] \#]))))

(def opposite {:up :down :left :right :right :left :down :up})

(defn make-dirs [dir pos]
  (dissoc {:up    (v+ pos -1 0)
           :down  (v+ pos 1 0)
           :left  (v+ pos 0 -1)
           :right (v+ pos 0 1)}
    (opposite dir)))

(defn calc-cost [dir1 dir2]
  (cond
    (= dir1 dir2)            1
    (= (opposite dir1) dir2) 2001
    :else                    1001))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defn update-search-state [{:keys [queue paths] :as _state} maze h-score]
  (let [[dir pos :as cur] (first (peek queue))
        updates
        (for [next-step (make-dirs dir pos)
              :let      [[new-dir new-pos] next-step]
              :when     (not= (get maze new-pos) \#)
              :let      [[old-cost path]    (get paths cur)
                         new-cost           (+ old-cost (calc-cost dir new-dir))
                         [known-cost _path] (get paths next-step)]
              :when     (< new-cost (or known-cost Integer/MAX_VALUE))]
          [next-step [new-cost (conj path next-step)]])]
    {:queue (into (pop queue)
              (map (fn [[state [cost _path]]]
                     [state (+ cost (h-score state))]))
              updates)
     :paths (into paths updates)}))

(defn solve-maze [fname]
  (let [maze        (parse fname)
        start-state [:right (:start maze)]
        end-pos     (:end maze)
        h-score     (fn [[_ pos]]
                      (manhattan-distance pos end-pos))]

    (loop [state {:queue (priority-map start-state 0)
                  :paths {start-state [0 []]}}]
      (let [[[_dir pos :as current] _cost] (peek (:queue state))]
        (cond
          (nil? pos)      nil
          (= pos end-pos) (first (get (:paths state) current))
          :else           (recur (update-search-state state maze h-score)))))))

(comment
  (def q (parse "example-16"))
  (solve-maze "example-16") ;; => 7036
  (solve-maze "input-16") ;; => 109496

  )
