(ns pagerank.normalise
  (:require [cascalog.api :refer :all]
            [cascalog.ops :as c]))

(defn norm [source]
  (let [weights (<- [?a ?weight]
                    (source ?a ?b ?score)
                    (c/sum ?score :> ?weight))]
    (<- [?a ?b ?normalised-weight]
        (weights ?a ?weight)
        (source ?a ?b ?score)
        (div ?score ?weight :> ?normalised-weight))))

