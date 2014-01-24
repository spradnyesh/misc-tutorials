(ns project-euler.prob003-test
  (:use expectations
        project-euler.prob003))

(expect 6857 (max-prime 600851475143))
