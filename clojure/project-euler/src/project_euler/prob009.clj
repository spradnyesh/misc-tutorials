(ns project-euler.prob009
  (:use [clojure.math.numeric-tower :as math]))

(defn is-pythagorean-triplet? [a b c]
  (if (and (< a b c)
           (= (math/expt c 2)
              (+ (math/expt a 2)
                 (math/expt b 2))))
    true
    false))

(defn pythagorean-triplet-product-with-sum [n]
  (first (for [a (range n),
               b (range n),
               c (range n)
               :when (and (= n (+ a b c))
                          (is-pythagorean-triplet? a b c))]
           (* a b c))))
