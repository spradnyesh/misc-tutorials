(ns project-euler.prob003
  (:use [clojure.math.numeric-tower :as math]))

(defn max-prime [n]
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

(comment ; not working, and (maybe) wrong approach
  (defn prime-factors-2 [n]
           (let [factors (atom [1])]
             (while (not= n (reduce * @factors))
               (do
                 (println "1: " (reduce * @factors))
                 (for [i (range 2 (inc (/ n 2)))]
                   (do (println "2: " i)
                       (when (= 0 (rem (/ n (reduce * @factors)) i))
                         (println "3: " @factors)
                         (swap! factors concat [i])
                         (println "3: " @factors))))))
             @factors)))

(defn is-prime? [n]
  (empty? (for [i (range 2 (math/sqrt (inc n)))
                :when (= 0 (rem n i))]
            i)))

(defn max-prime-2 [n]
  (apply max (for [i (range 2 (math/sqrt n))
                   :when (and (is-prime? i)
                              (= 0 (rem n i)))]
               i)))
