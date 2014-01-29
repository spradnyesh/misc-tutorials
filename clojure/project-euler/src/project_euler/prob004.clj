(ns project-euler.prob004
  (:use [clojure.string :as s]
        [clojure.math.numeric-tower :as math]))

(defn is-palindrome? [n]
  (let [str (str n)]
    (=  str (s/reverse str))))

(defn largest-palindrome [size]
  (apply max (for [i (range (math/expt 10 (dec size)) (math/expt 10 size))
             j (range (math/expt 10 (dec size)) (math/expt 10 size))
             :let [prod (* i j)]
             :when (is-palindrome? prod)]
         prod)))
