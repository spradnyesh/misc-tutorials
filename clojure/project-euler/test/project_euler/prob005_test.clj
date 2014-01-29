(ns project-euler.prob005-test
  (:use expectations
        project-euler.prob005))

(expect [2 2 5 5] (factors 100))
(expect 10 (gcd 10 20))
(expect 20 (lcm 10 20))
(expect (= (* 10 20) (* (gcd 10 20) (lcm 10 20))))
(expect 2520 (evenly-divisible min max))
