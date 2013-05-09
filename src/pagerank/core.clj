(ns pagerank.core
  (:require [cascalog.api :refer :all]
            [cascalog.ops :as c]
            [pagerank.normalise :refer [norm]]))

(defn instantiate-vector [source]
  (let [c (ffirst (??<- [?c]
                        (source ?a _ _)
                        (c/distinct-count :> ?c)))
        r (double (/ 1 c))]
    (<- [?a ?v]
        (source ?a _ _)
        (identity r :> ?v)
        (:distinct :true))))

(defn pr-iter [source vs]
  (<- [?i ?s]
      (source ?j ?i ?a)
      (vs ?j ?v)
      (* ?a ?v :> ?t)
      (c/sum ?t :> ?s)))

(defn rank [iterations source]
  (nth (iterate (partial pr-iter source)
                (instantiate-vector source))
       iterations))

;; (require '[pagerank.playground :refer [graph]])

;; (?- (stdout)
;;     (rank 10 (norm graph)))





