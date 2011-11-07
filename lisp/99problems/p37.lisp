(defpackage :99problems
  (:use :common-lisp
        :it.bese.FiveAM))
(in-package :99problems)

(def-suite encoded-prime-factors)
(in-suite encoded-prime-factors)
(test test-encode ()
      (is (equal '((A 4) (B 1) (C 2) (A 2) (D 1) (E 4))
                 (encode '(a a a a b c c a a d e e e e) 0))))
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
(test test-encoded-prime-factors ()
      (is (equal '((3 2) (5 1) (7 1)) (encoded-prime-factors 315)))
      (is (equal '((2 2) (79 1)) (encoded-prime-factors 316)))
      (is (equal '((3 1) (5 1) (31 1)) (encoded-prime-factors 465))))
(run! 'encoded-prime-factors)


(defun encode (lst count)
  (cond ((null lst)
         nil)
        ((eql (first lst)
              (first (rest lst)))
         (encode (rest lst) (incf count)))
        (t (append (list (list (first lst)
                               (incf count)))
                   (encode (rest lst) 0)))))
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
(defun prime-factors (n)
  (prime-factors-helper n 2))
(defun encoded-prime-factors (n)
  (encode (prime-factors n) 0))
(defun term (lst)
  (let ((p (first lst)))
    (* (1- p) (expt p (1- (second lst))))))
(defun phi-sum (lst)
  (if (null lst)
      0
      (+ (term (first lst))
         (phi-sum (rest lst)))))
(defun phi (n)
  (phi-sum (encoded-prime-factors n)))
