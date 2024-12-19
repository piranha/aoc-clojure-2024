(ns day19
  (:require [clojure.string :as str]))

(defn parse [fname]
  (let [[towels designs] (.split (slurp fname) "\n\n")]
    {:towels  (set (.split towels ", "))
     :designs (str/split-lines designs)}))

(def design?
  (memoize
    (fn [towels design]
      (if (str/blank? design)
        '()
        (first
          (for [towel towels
                :when (str/starts-with? design towel)
                :let  [res (design? towels (subs design (count towel)))]]
            (cons towel res)))))))

(defn part1 [fname]
  (let [{:keys [towels designs]} (parse fname)]
    (->> (filter (partial design? towels) designs)
         count)))

(def all-towels
  (memoize
    (fn [maxl towels design]
      (if (str/blank? design)
        1
        (->> (for [i     (range 1 (inc (min (count design) maxl)))
                   :let  [head (subs design 0 i)
                          tail (subs design i)]
                   :when (towels head)]
               (all-towels maxl towels tail))
             (reduce +))))))

(defn part2 [fname]
  (let [{:keys [towels designs]} (parse fname)
        maxl                     (apply max (map count towels))]
    (->> (map (partial all-towels maxl towels) designs)
         (reduce +))))

(comment
  (parse "example-19")
  (part1 "example-19") ;; => 6
  (user/time+ (part1 "input-19")) ;; => 369
  (part2 "example-19") ;; => 16
  (part2 "input-19") ;; => 761826581538190
  )
