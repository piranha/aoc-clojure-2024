(ns day12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn v+ [[i j] [di dj]]
  [(+ i di) (+ j dj)])

(defn in-bounds? [size [i j]]
  (and (< -1 i size)
       (< -1 j size)))

(defn make-dirs [pos]
  (map #(vector %2 (v+ pos %1))
    [[-1 0] [1  0] [0 -1] [0  1]]
    [:up    :down  :left  :right]))

(defn find-fences [start region]
  (set (for [[dir pos] (make-dirs start)
             :when     (not (contains? region pos))]
         [dir pos])))

(defn calc-peri [region]
  (into #{} (mapcat #(find-fences % region) region)))

(defn calc-region [region]
  {:area (count region)
   :peri (calc-peri region)})

(defn make-side-dirs [[dir pos]]
  (case dir
    :up    [[:up (v+ pos [0 1])]    [:up (v+ pos [0 -1])]]
    :down  [[:down (v+ pos [0 1])]  [:down (v+ pos [0 -1])]]
    :left  [[:left (v+ pos [1 0])]  [:left (v+ pos [-1 0])]]
    :right [[:right (v+ pos [1 0])] [:right (v+ pos [-1 0])]]))

(defn find-side [peri dirpos]
  (into #{dirpos} cat
    (for [side (make-side-dirs dirpos)
          :when (contains? peri side)]
      (find-side (disj peri dirpos) side))))

(defn find-sides [peri]
  (into #{} (map #(find-side peri %) peri)))

(comment
    (def example
    (mapv vec (str/split-lines
                "AAAA
BBCD
BBCC
EEEC")))

    (def q (-> (loop-regions example) :regions vals (nth 2) calc-region :peri))

    (count (find-sides q))

    (filter #(contains? q %) (make-side-dirs [:down [1 2]]))
)

(def *i (atom 0))

(defn check-position [{:keys [pos->id regions karta]} pos]
  (let [v       (get-in karta pos)
        up-id   (let [dir (v+ pos [-1 0])]
                  (when (= v (get-in karta dir))
                    (get pos->id dir)))
        left-id (let [dir (v+ pos [0 -1])]
                  (when (= v (get-in karta dir))
                    (get pos->id dir)))]
    (if (and up-id left-id (not= up-id left-id))
      {:karta   karta
       :pos->id (-> (assoc pos->id pos up-id)
                    (merge (zipmap (get regions left-id) (repeat up-id))))
       :regions (-> regions
                    (dissoc left-id)
                    (update up-id #(-> (conj % pos)
                                       (into (get regions left-id)))))}
      (let [id (or up-id left-id (str v (swap! *i inc)))]
        {:karta   karta
         :pos->id (assoc pos->id pos id)
         :regions (update regions id (fnil conj #{}) pos)}))))

(defn loop-regions [karta]
  (reset! *i 0)
  (let [size (count (first karta))]
    (reduce check-position
      {:pos->uuid {}
       :regions   {}
       :karta karta}
      (for [i (range size)
            j (range size)]
        [i j]))))

(defn parse [s]
  (mapv vec (str/split-lines (str/trim s))))

(defn twelve [fname]
  (let [karta (parse (slurp fname))
        res   (loop-regions karta)]
    ;;#p (:regions res)
    (->> (vals (:regions res))
         (map calc-region)
         (map #(* (:area %) (count (:peri %))))
         (reduce +))))

(defn twelve2 [fname]
  (let [karta (parse (slurp fname))
        res   (loop-regions karta)]
    ;;#p (:regions res)
    (->> (vals (:regions res))
         (map calc-region)
         (map #(* (:area %) (count (find-sides (:peri %)))))
         (reduce +))))

(comment

  (twelve "example-12") ;; => 1930
  (twelve "input-12") ;; => 1374934
  (twelve2 "example-12") ;; => 1206
  (twelve2 "input-12") ;; => 841078

  (->> (loop-regions example)
       :regions
       vals
       (map calc-region)))
