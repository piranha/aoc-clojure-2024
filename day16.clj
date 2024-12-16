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

(defn update-search-state [{:keys [queue costs] :as _state} maze h-score]
  (let [[dir pos] (first (peek queue))
        updates   (for [next-step (make-dirs dir pos)
                        :let      [[new-dir new-pos] next-step]
                        :when     (not= (get maze new-pos) \#)
                        :let      [old-cost   (get costs [dir pos])
                                   new-cost   (+ old-cost (calc-cost dir new-dir))
                                   known-cost (get costs next-step)]
                        :when     (< new-cost (or known-cost Integer/MAX_VALUE))]
                    [next-step new-cost])]
    {:queue (into (pop queue)
              (map (fn [[state cost]]
                     [state (+ cost (h-score state))]))
              updates)
     :costs (into costs updates)}))

(defn solve-maze [fname]
  (let [maze        (parse fname)
        start-state [:right (:start maze)]
        end-pos     (:end maze)
        h-score     (fn [[_ pos]]
                      (manhattan-distance pos end-pos))]

    (loop [state {:queue (priority-map start-state 0)
                  :costs {start-state 0}}]
      (let [[[_dir pos :as current] _cost] (peek (:queue state))]
        (cond
          (nil? pos) (:costs state)
          ;;(= pos end-pos) (get (:costs state) current)
          :else      (recur (update-search-state state maze h-score)))))))

(defn solve-all-maze [fname]
  (let [maze (parse fname)]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [:right (:start maze)])
           paths {[:right (:start maze)] [0 []]}]
      (if (empty? queue)
        paths
        (let [[dir pos :as cur] (peek queue)
              dirs              (->> (make-dirs dir pos)
                                     (remove #(= \# (get maze (second %)))))
              [cost path]       (get paths cur)]
          (recur
            (into (pop queue) dirs)
            (into paths (for [ndir dirs]
                          [ndir [(+ cost (calc-cost dir ndir)) (conj path ndir)]]))))))))

(comment
  (def q (parse "example-16"))
  (solve-maze "example-16") ;; => 7036
  (solve-maze "input-16") ;; => 109496

  (solve-all-maze "example-16")
  )
