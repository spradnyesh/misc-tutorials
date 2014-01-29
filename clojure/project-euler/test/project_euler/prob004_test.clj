(ns project-euler.prob004-test
  (:use expectations
        project-euler.prob004))

(expect true (is-palindrome? 9009))
(expect false (is-palindrome? 9010))

(expect 9009 (largest-palindrome 2))
(expect 906609 (largest-palindrome 3))
