(ns project-euler.prob001)
;; http://projecteuler.net/problem=1

(defn multiples [list of]
  (filter #(= 0 (rem % of)) list))

(defn merge-without-duplicates [& lists]
  (seq (set (reduce concat lists))))

(defn sum-multiples-below [n & args]
  (let [numbers (range n)]
    (reduce + (apply merge-without-duplicates (map #(multiples numbers %) args)))))
