(ns project-euler.prob003-test
  (:use expectations
        project-euler.prob003))

(expect 6857 (prime-factors 600851475143))
