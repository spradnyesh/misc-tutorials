(defpackage :99problems
  (:use :common-lisp
        :it.bese.FiveAM))
(in-package :99problems)
(test test-totient-phi ()
      (is (= 4 (totient-phi 10)))
      (is (= 1 (totient-phi 1))))
(run! 'test-totient-phi)

(defun is-coprime (a b)
  (when (= 1 (gcd a b)) t))
(defun totient-phi (a)
  (if (= 1 a)
      1
      (let ((phi 0))
        (loop
           for i from 1 to (1- a)
           do (when (is-coprime a i)
                (incf phi)))
        phi)))
