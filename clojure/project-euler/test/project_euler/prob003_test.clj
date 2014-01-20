(ns project-euler.prob003-test
  (:use expectations
        project-euler.prob003))

;; is-prime should return true/false based upon whether input is prime or not
(expect true (is-prime 3))
(expect false (is-prime 4))

;; next-prime should return prime number between the input
(expect 13 (next-prime 12 100))

;; prime-factors should return all prime factors
(expect [5 7 13 29] (prime-factors 13195))
