(defpackage :99problems
  (:use :common-lisp
        :it.bese.FiveAM))
(in-package :99problems)
(test test-co-prime ()
      (is (null (is-coprime 36 63)))
      (is (is-coprime 78 95)))
(defun is-coprime (a b)
  (when (= 1 (gcd a b)) t))

(run! 'test-co-prime)
