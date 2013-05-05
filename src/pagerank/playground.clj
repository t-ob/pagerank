(ns pagerank.playground)

(def pages
  (map (partial str "page:")
       (range 100 400)))

(def pairs
  (for [p1 pages p2 pages :when (not= p1 p2)]
    [p1 p2]))

(take 10 pairs)

(defn tails [c]
  (take-while not-empty
              (iterate rest c)))


(defn tob-pairs [[x & xs :as c] [y & ys :as d]]
  (cond
   (nil? x) nil
   (nil? y) nil
   :else    (into (tob-pairs xs ys)
                  (for [y d]
                    [x y]))))

(def pairs
  (remove (fn [[a b]] (= a b))
          (tob-pairs pages pages)))

(def graph-input
  (remove nil?
          (let [p 0.01]
            (for [[p1 p2] pairs]
              (when (< (rand) p)
                (let [affinity (/ (rand) 100)]
                  [[p1 p2 affinity] [p2 p1 affinity]]))))))

(def graph
  (reduce into [] graph-input))

(defn mat-col-mult [matrix col]
  (for [row matrix]
    (apply +
           (map * row col))))

(defn tob-pr [lim matrix]
  (let [c (count matrix)
        v (repeat c (/ 1 c))]
    (nth (iterate (partial mat-col-mult
                           matrix)
                  v)
         lim)))


(let [matrix [[1/3 1/2 0]
              [1/3 0 1/2]
              [1/3 1/2 1/2]]
      v0     [1/3 1/3 1/3]]
  (map double (tob-pr 75 matrix)))




