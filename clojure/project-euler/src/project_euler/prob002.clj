(ns project-euler.prob002)
;; http://projecteuler.net/problem=2

(defn fib [n]
  (if (<= n 1)
    1
    (+ (fib (dec n)) (fib (- n 2)))))

(def fib (memoize fib))

(defn fib-even-sum [n]
  (reduce + (filter even? (rest (take-while #(< % n) (map fib (range 1 100000)))))))
