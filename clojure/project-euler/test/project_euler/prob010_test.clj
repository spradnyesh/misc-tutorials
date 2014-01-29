(ns project-euler.prob010-test
  (:use expectations
        project-euler.prob010))

(expect [2 3 5 7] (primes-below 10))
(expect 17 (prime-sum 10))
