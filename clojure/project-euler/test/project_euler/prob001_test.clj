(ns project-euler.prob001-test
  (:use expectations
        project-euler.prob001))

;; multiples of 2 in list of (range 10) should return [0 2 4 6 8]
(expect [0 2 4 6 8] (multiples (range 10) 2))

;; merging 2 lists should remove duplicates
(expect [3 5 6 9 10 12 15 18] (merge-without-duplicates [3 6 9 12 15 18] [5 10 15]))

;; sum-multiples should return sum of multiples
(expect 23 (sum-multiples-below 10 3 5))
