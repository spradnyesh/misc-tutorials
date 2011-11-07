(defpackage :99problems
  (:use :common-lisp
        :it.bese.FiveAM))
(in-package :99problems)

(def-suite istree)
(in-suite istree)
(test test-istree ()
      (is (istree '(a (b nil nil) nil)))
      (is (null (istree '(a (b nil nil))))))
(run! 'istree)

(defun istree (lst)
  (cond ((null lst) t)
        ((and (listp lst)
              (= 3 (length lst))
              (istree (second lst))
              (istree (third lst))))
        (t nil)))
