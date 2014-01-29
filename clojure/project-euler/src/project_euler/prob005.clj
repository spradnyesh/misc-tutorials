(ns project-euler.prob005
  (:use [clojure.math.numeric-tower :as math :only (sqrt)]))

;; copied shamelessly from http://stackoverflow.com/a/3080718 :(
(defn gcd
  ([x y]
     (cond (zero? x) y
           (< y x)   (recur y x)
           :else     (recur x (rem y x))))
  ([x y & zs]
     (reduce gcd (gcd x y) zs)))
(defn lcm
  ([x y] (/ (* x y) (gcd x y)))
  ([x y & zs]
     (reduce lcm (lcm x y) zs)))

(defn evenly-divisible [min max]
  (apply lcm (range min max)))
