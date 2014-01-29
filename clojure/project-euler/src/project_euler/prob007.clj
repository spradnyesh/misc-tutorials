(ns project-euler.prob007
  (:use [clojure.math.numeric-tower :as math]))

(defn is-prime? [n]
  (empty? (for [i (range 2 (math/sqrt (inc n)))
                :when (= 0 (rem n i))]
            i)))

(defn next-prime [n]
  (if (< n 2)
    [2]
    (take 1 (filter is-prime? (iterate inc (inc n))))))

(defn nth-prime [n]
  (let [p (atom 1)]
    (dotimes [i n]
      (swap! p #(do %2) (first (next-prime @p))))
    @p))
