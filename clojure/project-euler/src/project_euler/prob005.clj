(ns project-euler.prob005
  (:use [clojure.math.numeric-tower :as math :only (sqrt)]))

(defn is-prime? [n]
  (empty? (for [i (range 2 (math/sqrt (inc n)))
                :when (= 0 (rem n i))]
            i)))
(defn factors [n]
  (sort (loop [num n
               factors []]
          (if (= 1 num)
            factors
            (let [partial-factors (if (is-prime? num)
                                    (list num)
                                    (for [i (range 2 num)
                                              :when (and (is-prime? i)
                                                         (= 0 (rem num i)))]
                                          i))]
              (recur (/ num (reduce * partial-factors))
                     (concat factors partial-factors)))))))
(defn lcm [a b]
  (let [a-factors (factors a)
        b-factors (factors b)])
  )
(defn gcd [a b]
  (/ (* a b) (lcm a b)))
(defn evenly-divisible [min max]
  (lcm min max))
