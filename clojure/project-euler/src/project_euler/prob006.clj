(ns project-euler.prob006
  (:use [clojure.math.numeric-tower :as math]))

(defn sum-of-squares [min max]
  (reduce + (map #(math/expt % 2) (range min (inc max)))))

(defn square-of-sums [min max]
  (math/expt (reduce + (range min (inc max))) 2))

(defn diff-of-sums-and-squares [min max]
  (- (square-of-sums min max)
     (sum-of-squares min max)))
