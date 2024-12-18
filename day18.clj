(ns day18
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn v+ [[y x] di dj]
  [(+ y di) (+ x dj)])

(defn parse [fname]
  (->> (slurp fname)
       (str/split-lines)
       (mapv #(mapv parse-long (.split % ",")))))

(defn make-dirs [pos]
  [(v+ pos 0 -1)
   (v+ pos 0  1)
   (v+ pos -1 0)
   (v+ pos 1  0)])

(defn in-bounds? [size [x y]]
  (and (< -1 x size)
       (< -1 y size)))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defn update-search-state [{:keys [queue paths] :as _state} maze size h-score]
  (let [cur (first (peek queue))
        updates
        (for [new-pos (make-dirs cur)
              :when   (and (not (contains? maze new-pos))
                           (in-bounds? size new-pos))
              :let    [[old-cost path]    (get paths cur)
                       new-cost           (inc old-cost)
                       [known-cost _path] (get paths new-pos)]
              :when   (< new-cost (or known-cost Integer/MAX_VALUE))]
          [new-pos [new-cost (conj path new-pos)]])]
    {:queue (into (pop queue)
              (map (fn [[state [cost _path]]]
                     [state (+ cost (h-score state))]))
              updates)
     :paths (into paths updates)}))


(defn part1 [fname]
  (let [maze     (parse fname)
        size     (if (< (count maze) 30)
                   7
                   71)
        how-many (if (< (count maze) 30)
                   12
                   1024)
        maze     (set (take how-many maze))
        start    [0 0]
        end      [(dec size) (dec size)]
        h-score  (partial manhattan-distance end)]
    (loop [state {:queue (priority-map start 0)
                  :paths {start [0 []]}}]
      (let [[pos _cost] (peek (:queue state))]
        (cond
          (nil? pos)  nil
          (= pos end) (first (get (:paths state) pos))
          :else       (recur (update-search-state state maze size h-score)))))))

(defn part2 [fname]
  (let [obs      (parse fname)
        size     (if (< (count obs) 30)
                   7
                   71)
        how-many (if (< (count obs) 30)
                   12
                   1024)
        start    [0 0]
        end      [(dec size) (dec size)]
        h-score  (partial manhattan-distance end)]
    (loop [how-many how-many]
      (let [maze (set (take how-many obs))
            res  (loop [state {:queue (priority-map start 0)
                               :paths {start [0 []]}}]
                   (let [[pos _cost] (peek (:queue state))]
                     (cond
                       (nil? pos)  nil
                       (= pos end) (first (get (:paths state) pos))
                       :else       (recur (update-search-state state maze size h-score)))))]
        (cond
          (nil? res)               (get obs (dec how-many))
          (= how-many (count obs)) nil
          :else                    (recur (inc how-many)))))))


(comment
  (def q (parse "example-18"))
  (part1 "example-18") ;; => 22
  (part1 "input-18") ;; => 250
  (part2 "example-18") ;; => [6 1]
  (part2 "input-18") ;; => [56 8]

  (part1 "18-vsevolod-input") ;; => 262
  (part2 "18-vsevolod-input") ;; => [22 20]
  )
