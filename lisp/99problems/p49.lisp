(defpackage :99problems
  (:use :common-lisp
        :it.bese.FiveAM))
(in-package :99problems)

(def-suite gray-codes)
(in-suite gray-codes)
(test test-gray ()
      (is (equal '("0" "1") (gray-code 1)))
      (is (equal '("11" "01" "10" "00") (gray-code 2))))
(run! 'gray-codes)

(defun gray-code (n)
  (cond ((> 1 n) nil)
        ((= 1 n)
         (list "0" "1"))
        (t (let ((lst nil))
             (dolist (i (gray-code (1- n)))
               (setf lst (cons (concatenate 'string "0" i) lst))
               (setf lst (cons (concatenate 'string "1" i) lst)))
             lst))))
