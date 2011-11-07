(defpackage :99problems
  (:use :common-lisp
        :it.bese.FiveAM))
(in-package :99problems)
(test test-gcd ()
      (is (= 9 (my-gcd 36 63)))
      (is (= 5 (my-gcd 25 35)))
      (is (= 2 (my-gcd 36 62)))
      (is (= 1 (my-gcd 8 65)))
      (is (= 1 (my-gcd 78 95))))
(defun my-gcd (a b)
  (if (< a b)
      (my-gcd b a)
      (let ((m (mod a b)))
        (if (zerop m)
            b
            (my-gcd b m)))))

(run! 'test-gcd)
