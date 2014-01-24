(ns project-euler.prob003
  (:require [clojure.math.numeric-tower :as math]))

(defn prime-factors [n]
  (let [num (atom n)
        factor (atom 2)
        last-factor (atom 1)]
    (while (> @num 1)
      (when (= 0 (rem @num @factor))
        (swap! last-factor #(do %2) @factor)
        (while (= 0 (rem @num @factor))
          (swap! num / @factor)))
      (swap! factor inc))
    @last-factor))
