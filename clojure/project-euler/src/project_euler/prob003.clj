(ns project-euler.prob003)

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

(comment (defn prime-factors-2 [n]
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

(defn max-prime-2 [n]
  (max (loop [num n,
              factors [1]]
         (if (= n (reduce * @factors))
           @factors
           (let [sub-factors (concat factors (for [i (range 2 (inc (/ num 2)))
                                                   :when (= 0 (rem num i))]
                                               i))]
             (recur (/ num (reduce * sub-factors)) sub-factors))))))
