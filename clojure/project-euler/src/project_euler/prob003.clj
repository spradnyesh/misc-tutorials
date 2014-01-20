(ns project-euler.prob003
  (:require [clojure.math.numeric-tower :as math]))

(defn is-prime [n]
  (if (<= n 3)
    true
    (do
      (for [i (range 2 (math/floor (math/sqrt n)))
            :while (= 0 (rem n i))]
        true)
      false)))

(defn next-prime
  ([min] (next-prime min 999999))
  ([min max]
     (first (for [i (range (inc min) max)
                  :when (= true (is-prime i))]
              i))))

(defn ith-prime [i]
  (if (= i 0)
    1
    (next-prime (ith-prime (dec i)))))

(def ith-prime (memoize ith-prime))

(defn prime-factors [n]
  )
