(ns project-euler.prob008-test
  (:use expectations
        project-euler.prob008))

(expect 120 (str-prod "12345"))
(expect [3 4 5] (splice [1 2 3 4 5 6 7] 2 3))
(expect 20 (largest-product 12345 2))
