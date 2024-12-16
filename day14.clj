(ns day14
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [io.github.humbleui.canvas :as canvas]
            [io.github.humbleui.ui :as ui]
            [io.github.humbleui.window :as window]
            [io.github.humbleui.util :as util]
            [util :as u]))

(defn v+ [[x y] i j]
  [(+ x i) (+ y j)])

(defn parse [s]
  (vec
    (for [line (str/split-lines s)]
      (let [[px py vx vy] (map parse-long (re-seq #"-?\d+" line))]
        {:p [px py] :v [vx vy]}))))

(defn calc-step [{[px py] :p [vx vy] :v} steps [w h]]
  [(-> (* vx steps) (+ px) (mod w))
   (-> (* vy steps) (+ py) (mod h))])

(defn calc-frame [robots steps size]
  (map #(calc-step % steps size) robots))

(defn calc-q [[x y] [w h]]
  (let [xr (/ x (/ (dec w) 2))
        yr (/ y (/ (dec h) 2))]
    (if (or (= xr 1) (= yr 1))
      [nil nil]
      [(min 1 (int xr)) (min 1 (int yr))])))

(defn split-qs [robots size]
  (group-by #(calc-q % size) robots))

(defn fourteen [fname]
  (let [robots (parse (slurp fname))
        size   (if (str/starts-with? fname "example")
                 [11 7]
                 [101 103])]
    (-> (calc-frame robots 100 size)
        (split-qs size)
        (dissoc [nil nil])
        (update-vals count)
        vals
        (->> (reduce *)))))

(deftest fourteen-test
  (is (= [1 3] (calc-step {:p [2 4] :v [2 -3]} 5 [11 7]))))

(defn longest-consecutive-sequence
  [nums]
  (let [num-set (set nums)]
    (reduce (fn [acc num]
              ;; Only process numbers that are the start of a sequence
              (if (contains? num-set (dec num))
                acc
                (let [end+1  (loop [n (inc num)]
                               (if (contains? num-set n)
                                 (recur (inc n))
                                 n))
                      length (- end+1 num)]
                  (max length acc))))
      0 nums)))

(defn longest-hline [robots]
  (->> (u/group-by first second robots)
       (map #(longest-consecutive-sequence (val %)))
       (apply max)))

(defn cluster? [robots pos]
  (and (contains? robots (v+ pos  1  0))
       (contains? robots (v+ pos -1  0))
       (contains? robots (v+ pos  0  1))
       (contains? robots (v+ pos  0 -1))))

(defn cluster-cnt [robots]
  (transduce (map #(if (cluster? robots %) 1 0)) + 0 robots))

(comment
  (def q (parse (slurp "example-14")))
  (def w (parse (slurp "input-14")))

  (def g (parse (slurp "input-igor-14")))

  (let [r (set (calc-frame g 6398 [103 101]))]
    (contains? r [38 50])#_
    (cluster? r [55 53]))

  (->> (calc-frame g 6516 [103 101])
       set
       cluster-cnt)

  (->> (calc-frame g 6515 [103 101])
       (u/group-by first second)
       (sort-by (comp - count second))
       #_
       (map #(longest-consecutive-sequence (val %))))

  (time
    (apply max-key second
      (for [i    (range 10000)
            :let [robots (calc-frame w i [103 101])]]
        [i (cluster-cnt robots)])))

  (fourteen "example-14") ;; => 12
  (fourteen "input-14") ;; => 219512160
  )


;;; part 2

(defonce *window (atom nil))

(def input (parse (slurp "input-14")))
(def field (if (< 20 (count input))
            [101 103]
            [11 7]))

(def *robots
  (ui/signal input))

(defonce *time
  (ui/signal {:text "0"}))

(def *playing?
  (ui/signal false))

(def *hover-pos
  (ui/signal nil))

(defn s+ [v delta]
  (+ (parse-long v) delta))

(defn step-time [delta]
  (swap! *time update :text #(-> (s+ % delta) (max 0) str)))

(defn render-robots [ctx canvas size]
  (let [robots @*robots
        t      (or (some-> (:text @*time) not-empty parse-long) 0)
        scale  (min (/ (:width size) (first field))
                 (/ (:height size) (second field)))]
    (canvas/with-canvas canvas
      (canvas/scale canvas scale scale)
      (with-open [red   (ui/paint {:fill 0xFFC41E3A} ctx)
                  green (ui/paint {:fill 0xFF15673A} ctx)]
        (doseq [[i robot] (->> (calc-frame robots t field)
                               (map-indexed vector))]
          (let [[x y] robot]
            (canvas/draw-rect canvas (util/rect-xywh x y 1 1)
              (get [red green] (mod i 2))
              #_
              (case (mod i 2)
                0 red
                1 green)))))
      (when-some [[x y] @*hover-pos]
        (with-open [paint (ui/paint {:stroke 0xFF00FFFF :width (/ 3 scale)} ctx)]
          (canvas/draw-rect canvas
            (util/rect-xywh (Math/floor x) (Math/floor y) 1 1)
            paint))))))

(defn canvas []
  [ui/stack
   [ui/canvas
    {:on-paint render-robots
     :on-event (fn [_ctx e]
                 (when (= :mouse-move (:event e))
                   (let [scale (min
                                 (/ (:width (:bounds ui/*node*)) 101)
                                 (/ (:height (:bounds ui/*node*)) 103))
                         x     (/ (:x e) scale)
                         y     (/ (:y e) scale)]
                     (reset! *hover-pos [(int x) (int y)]))))}]
   (let [[x y] (or @*hover-pos [0 0])]
     [ui/overlay
      [ui/padding {:left 0 :top 0}
       [ui/rect {:paint {:fill 0xFFFFFFFF}}
        [ui/padding {:padding 10}
         [ui/label (format "x=%s y=%s" x y)]]]]])])

(defn play-pause-btn []
  (let [cancel (util/schedule #(when @*playing?
                                 (step-time 1)
                                 (window/request-frame @*window))
                 0 33)]
    {:after-unmount cancel
     :render
     (fn []
       [ui/button
        {:on-click (fn [_e] (swap! *playing? not))}
        [ui/label (if @*playing? "⏸" "▶")]])}))

(defn app []
  [ui/default-theme
   [ui/key-listener {:on-key-down
                     (fn [e]
                       (case (:key e)
                         (:up :right)  (step-time 1)
                         (:down :left) (step-time -1)
                         nil))}
    [ui/padding {:padding 10}
     [ui/column {:gap 10}
      [ui/row {:gap 10}
       [ui/align {:y :center}
        [ui/label "Frame:"]]
       [ui/size {:width 40}
        [ui/text-field {:*state *time :width 40}]]
       [play-pause-btn]]
      ^{:stretch 1}
      [canvas]]]]])

(comment
  (ui/start-app!
    (reset! *window (ui/window {:title "Robots" :width 800 :height 800} #'app))))
