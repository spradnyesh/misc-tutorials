(ns project-euler.prob008
  (:use [clojure.math.numeric-tower :as math]))

(defn str-prod [s]
  (reduce * (map #(Integer/parseInt (str %)) s)))

(defn splice [list from length]
  (for [i (range from (+ from length))]
    (nth list i)))

(defn largest-product [n length]
  (let [str (str n)
        largest (atom 0)]
    (dotimes [i (- (count str) (dec length))]
      (let [sp (splice str i length),
            prod (str-prod sp)]
        (when (> prod @largest)
          (swap! largest #(do %2) prod))))
    @largest))
