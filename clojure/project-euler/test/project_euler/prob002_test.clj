(ns project-euler.prob002-test
  (:use expectations
        project-euler.prob002))

;; fib n should return n-th fibonacci number
(expect 1 (fib 1))
(expect 2 (fib 2))
(expect 89 (fib 10))

;; fib-even-sum should return sum of every even-valued term below n
(expect 44 (fib-even-sum 100)) ; 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ... => (+ 2 8 34) => 44
(expect 4613732 (fib-even-sum 4000000))
