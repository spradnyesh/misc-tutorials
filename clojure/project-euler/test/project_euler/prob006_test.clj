(ns project-euler.prob006-test
  (:use expectations
        project-euler.prob006))

(expect 385 (sum-of-squares 1 10))
(expect 3025 (square-of-sums 1 10))
(expect 2640 (diff-of-sums-and-squares 1 10))
