(ns project-euler.prob007-test
  (:use expectations
        project-euler.prob007))

(expect 2 (nth-prime 1))
(expect 541 (nth-prime 100))
