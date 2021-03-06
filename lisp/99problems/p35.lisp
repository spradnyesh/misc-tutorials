(defpackage :99problems
  (:use :common-lisp
        :it.bese.FiveAM))
(in-package :99problems)

(def-suite prime-factors)
(in-suite prime-factors)
(test test-is-prime ()
      (is (null (is-prime 4)))
      (is (null (is-prime 0)))
      (is (is-prime 1))
      (is (is-prime 7)))
(test test-next-prime ()
      (is (= 3 (next-prime 2)))
      (is (= 5 (next-prime 3))))
(test test-prime-factors ()
      (is (equal '(3 3 5 7) (prime-factors 315)))
      (is (equal '(2 2 79) (prime-factors 316)))
      (is (equal '(3 5 31) (prime-factors 465))))
(run! 'prime-factors)

(defun is-prime-helper (n k s)
  (cond ((= k s) t)
        ((and (> k 1)
              (zerop (mod n k)))
         nil)
        (t (is-prime-helper n (incf k) s))))
(defun is-prime (n)
  (is-prime-helper n 2 (1+ (floor (sqrt n)))))
(defun next-prime (k)
  (if (is-prime (1+ k))
      (1+ k)
      (next-prime (1+ k))))
(defun prime-factors-helper (a prime)
  (if (= a prime)
      (list a)
      (if (zerop (mod a prime))
          (cons prime (prime-factors-helper (/ a prime) prime))
          (prime-factors-helper a (next-prime prime)))))
(defun prime-factors (a)
  (prime-factors-helper a 2))
