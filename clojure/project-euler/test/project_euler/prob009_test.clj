(ns project-euler.prob009-test
  (:use expectations
        project-euler.prob009))

(expect true (is-pythagorean-triplet? 3 4 5))
(expect false (is-pythagorean-triplet? 2 4 5))
(expect false (is-pythagorean-triplet? 4 3 5))
(expect 60 (pythagorean-triplet-product-with-sum 12))
(expect nil (pythagorean-triplet-product-with-sum 100))
