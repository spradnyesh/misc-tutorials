(ns project-euler.prob003-test
  (:use expectations
        project-euler.prob003))

(expect true (is-prime? 47))
(expect false (is-prime? 50))
(expect 6857 (max-prime 600851475143))
(expect 6857 (max-prime-2 600851475143))
