(ns string-calculator.core-test
  (:use expectations
        string-calculator.core))

;; empty string should return 0
(expect 0 (sum-of-strings ""))

;; string with single number should return number
(expect 1 (sum-of-strings "1"))

;; comma separated numbers should be summed
(expect 3 (sum-of-strings "1,2"))

;; unknown amount of numbers should be summed
(expect 15 (sum-of-strings "1,2,3,4,5"))

;; new-line should be treated like a separator
(expect 6 (sum-of-strings "1\n2,3"))

;; custom delimiters should be treated like a separator
(expect 3 (sum-of-strings "//;\n1;2"))

;; numbers bigger than 1000 should be ignored
(expect 2 (sum-of-strings "1001,2"))
(expect 1002 (sum-of-strings "1000,2"))

;; multiple-character length strings should be treated like a separator
(expect 6 (sum-of-strings "//[***]\n1***2***3"))

;; multiple delimeters should be allowed
(expect 6 (sum-of-strings "//[*][%]\n1*2%3"))

;; allow multiple-character multiple delimeters
(expect 6 (sum-of-strings "//[***][%]\n1***2%3"))

;; throw exception for negative numbers
(expect Exception (sum-of-strings "1,-1"))