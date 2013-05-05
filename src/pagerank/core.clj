(ns pagerank.core
  (:require [cascalog.api :refer :all]
            [cascalog.ops :as c]
            [pagerank.normalise :refer [norm]]))

(defn pr-iter [source vs]
  (<- [?i ?s]
      (source ?j ?i ?a)
      (vs ?j ?v)
      (* ?a ?v :> ?t)
      (c/sum ?t :> ?s)))

(defn rank [iterations source vs]
  (nth (iterate (partial pr-iter source)
                (pr-iter source vs))
       iterations))

