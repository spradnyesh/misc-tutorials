(ns project-euler.prob010
  (:use [clojure.math.numeric-tower :as math]))

(defn is-prime? [n]
  (empty? (for [i (range 2 (math/sqrt (inc n)))
                :when (= 0 (rem n i))]
            i)))

(defn primes-below [n]
  (for [i (range 2 n)
        :when (is-prime? i)]
    i))

(defn prime-sum [n]
  (reduce + (primes-below n)))
