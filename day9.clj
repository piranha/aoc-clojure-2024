(ns day9
  (:require [clojure.string :as str]
            [io.github.humbleui.ui :as ui]
            [io.github.humbleui.canvas :as canvas]))

(defn char->digit [c]
  (- (int c) 48))

(defn parse9 [fname]
  (->> (map char->digit (str/trim (slurp fname)))
       (partition-all 2)
       (map-indexed (fn [i [flen slen]]
                      #_
                      [[i flen] [nil slen]]
                      (concat (repeat flen i) (repeat (or slen 0) -1))))
       (into [] cat)
       #_
       (remove #(or (nil? (second %)) (zero? (second %))))
       vec))

(defn defrag [disk]
  (let [arr (int-array (into [] disk))]
    (loop [i 0
           j (dec (count arr))]
      (cond
        (>= i j)              (vec arr)
        (not= (get arr i) -1) (recur (inc i) j)
        (= (get arr j) -1)    (recur i (dec j))
        :else                 (do (aset-int arr i (get arr j))
                                  (aset-int arr j -1)
                                  (recur (inc i) (dec j)))))))

(defn same-end [disk-arr i]
  (let [v (get disk-arr i)]
    (loop [i i]
      (if (= (get disk-arr i) v)
        (recur (inc i))
        i))))

(defn same-start [disk-arr j]
  (let [v (get disk-arr (dec j))]
    (loop [j j]
      (if (= (get disk-arr (dec j)) v)
        (recur (dec j))
        j))))

(defn swap-file [disk-arr [s-start _s-end] [f-start f-end]]
  (doseq [offset (range (- f-end f-start))]
    (aset-int disk-arr (+ s-start offset) (get disk-arr (+ f-start offset)))
    (aset-int disk-arr (+ f-start offset) -1)))

(defn suitable? [[s-start s-end] [f-start f-end]]
  (>= (- s-end s-start) (- f-end f-start)))

(defn defrag-whole [disk]
  (let [arr (int-array (into [] disk))]
    (loop [i 0
           j (count arr)]
      (let [space-end  (same-end arr i)
            file-start (same-start arr j)]
        (cond
          (or (zero? j)
              (zero? file-start))  (vec arr)
          (>= i j)                 (recur 0 file-start)
          (not= (get arr i) -1)    (recur (inc i) j)
          (= (get arr (dec j)) -1) (recur i (dec j))
          :else                    (if (suitable? [i space-end] [file-start j])
                                     (do (swap-file arr [i space-end] [file-start j])
                                         (recur 0 file-start))
                                     (recur (inc space-end) j)))))))

(defn checksum [disk]
  (->> disk
       (map-indexed #(if (= -1 %2) 0 (* %1 %2)))
       (reduce +)))

(defn nine [fname]
  (-> (parse9 fname) defrag checksum))

(defn prn9 [v]
  (->> v
       (map #(if (= % -1) "." %))
       (apply str)))

(defn parse92 [fname]
  (->> (map char->digit (str/trim (slurp fname)))
       (partition-all 2)
       (map-indexed (fn [i [flen slen]]
                      [[i flen] [nil slen]]))
       (into [] cat)
       (remove #(or (nil? (second %)) (zero? (second %))))
       vec))


;;; approach dealing with disk-parts rather than spans of numbers

(defn collapse
  "Creates a transducer that collapses consecutive elements based on a predicate.
   pred - a function taking two arguments (previous-element, current-element)
   that returns true if they should be collapsed
   collapse-fn - a function taking two arguments that returns the collapsed result"
  [pred collapse-fn]
  (fn [rf]
    (let [prev (volatile! ::none)]
      (fn
        ([] (rf))
        ([result]
         (let [result (if (= @prev ::none)
                       result
                       (rf result @prev))]
           (rf result)))
        ([result input]
         (if (= @prev ::none)
           (do (vreset! prev input)
               result)
           (if (pred @prev input)
             (do (vreset! prev (collapse-fn @prev input))
                 result)
             (let [result (rf result @prev)]
               (vreset! prev input)
               result))))))))

(def collapse-space (collapse
                      #(= (first %1) (first %2))
                      (fn [[v l1] [_v l2]] [v (+ l1 l2)])))

(def drop-space (collapse
                  #(= 0 (second %2))
                  (fn [a _b] a)))

(defn move-file [disk space-idx file-idx]
  (let [s     (get disk space-idx)
        f     (get disk file-idx)
        delta (- (second s) (second f))]
    (into [] cat #_(comp cat collapse-space drop-space)
      [(subvec disk 0 space-idx)
       [f [nil delta]]
       (subvec disk (inc space-idx) file-idx)
       [[nil (second f)]]
       (subvec disk (inc file-idx))])))

(comment
  (move-file [[1 2] [nil 2] [3 1]] 1 2))

(defn defrag-whole2 [disk]
  (loop [i    0
         j    (dec (count disk))
         disk disk]
    (let [s (get disk i)
          f (get disk j)]
      (cond
        (zero? j)            disk
        (>= i j)             (recur 0 (dec j) disk)
        (not= (first s) nil) (recur (inc i) j disk)
        (= (first f) nil)    (recur i (dec j) disk)
        :else                (let [delta (- (second s) (second f))]
                               (if (neg? delta)
                                 (recur (inc i) j disk)
                                 (recur 0 (dec j) (move-file disk i j))))))))

(defn checksum2 [disk]
  (first
    (reduce
      (fn [[acc offset] [file-id len]]
        (let [new-offset (+ offset len)]
          [(if (nil? file-id)
             acc
             (reduce + acc (map * (repeat file-id) (range offset new-offset))))
           new-offset]))
      [0 0]
      disk)))

(defn nine2 [fname]
  (-> (parse92 fname) defrag-whole2 checksum2))

(comment
  (def q (int-array [-1 3 -1]))
  (def fname "input-9")
  (def q (parse92 fname))
  (def x (defrag-whole2 q))
  (checksum2 x) ;; => 6401189647890 -- too high
  (nine2 "example-9")

;; => [0 0 -1 -1 -1 1 1 1 -1 -1 -1 2 -1 -1 -1 3 3 3 -1 4 4 -1 5 5 5 5 -1 6 6 6 6 -1 7 7 7 -1 8 8 8 8 9  9]
;; => [0 0 9  9  -1 1 1 1  7  7  7 2 4 4 -1 3 3 3 -1 -1 -1 -1 5 5 5 5 -1 6 6 6 6 -1 -1 -1 -1 -1 8 8 8 8 -1 -1]
  ;; => [0 0 9 9 2 1 1 1 7 7 7 -1 4 4 -1 3 3 3 -1 -1 -1 -1 5 5 5 5 -1 6 6 6 6 -1 -1 -1 -1 -1 8 8 8 8 -1 -1]
  ;; "00992111777.44.333....5555.6666.....8888.."
  (def x (defrag-whole q))
  (same-end q 2)
  (same-start q 42)
  (nine "input-9") ;; => 6323641412437
  (nine2 "example-9")
  )


;;; ui

(def dim 80)

(defn paint [ctx canvas size]
  #p size
  (let [field (min (:width size) (:height size))
        scale (/ field dim)]
    ;; center canvas
    (canvas/translate canvas
      (-> (:width size) (- field) (/ 2))
      (-> (:height size) (- field) (/ 2)))
    ;; scale to fit full width/height but keep square aspect ratio
    (canvas/scale canvas scale scale)
    ))

(ui/defcomp app []
  [ui/default-theme
   [ui/center
    [ui/canvas {}
     #_
     {:on-paint paint}]]])

(defonce *window
  (atom nil))

(comment
  (ui/start-app!
    (reset! *window (ui/window {:title "Defrag" :width 800 :height 800} #'app)))

  (def disk-data [::fid ::fid ::fid nil nil nil ::fid2 ::fid2 ::fid2 nil])

  )

