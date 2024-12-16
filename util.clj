(ns util)

(defn group-by
  "(group-by first                  [[1 3]   [1 4]   [2 5]])   => {1 [[1 3] [1 4]], 2 [[2 5]]}
   (group-by first second           [[1 3]   [1 4]   [2 5]])   => {1 [3 4],         2 [5]}
   (group-by first second +      0  [[1 3]   [1 4]   [2 5]])   => {1 7,             2 5}
   (group-by first second           [[1 [3]] [1 [4]] [2 [5]]]) => {1 [[3] [4]],     2 [[5]]}
   (group-by first second concat    [[1 [3]] [1 [4]] [2 [5]]]) => {1 (3 4),         2 (5)}
   (group-by first second into      [[1 [3]] [1 [4]] [2 [5]]]) => {1 [3 4],         2 [5]}
   (group-by first second into   [] [[1 [3]] [1 [4]] [2 [5]]]) => {1 [3 4],         2 [5]}
   (group-by first second into   () [[1 [3]] [1 [4]] [2 [5]]]) => {1 (4 3),         2 (5)}
   ;; as a filter:
             kf    kpred  vf     vpred rf   init
   (group-by first any?   second even? conj () [[1 3] [1 4] [2 5]])      => {1 (4)}
   ;; as a reducer (see index-by below):
             kf    kpred  vf     vpred rf   init
   (group-by first any?   second even? max  0  [[1 3] [1 6] [1 4] [2 5]] => {1 6})"

  ([kf coll] (clojure.core/group-by kf coll))
  ([kf vf coll] (group-by kf vf conj [] coll))
  ([kf vf rf coll] (group-by kf vf rf [] coll))
  ([kf vf rf init coll]
   (->> coll
        (reduce
         (fn [m x]
           (let [k (kf x)]
             (assoc! m k (rf (get m k init) (vf x)))))
         (transient {}))
        (persistent!)))
  ([kf kpred vf vpred rf init coll]
   (->> coll
        (reduce
         (fn [m x]
           (let [k (kf x)]
             (if-not (kpred k)
               m
               (let [v (vf x)]
                 ;; no empty collections as a 'side effect':
                 (if-not (vpred v)
                   m
                   (assoc! m k (rf (get m k init) v)))))))
         (transient {}))
        (persistent!))))
