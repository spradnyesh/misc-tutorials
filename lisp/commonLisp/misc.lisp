(defpackage :calculator
  (:use :common-lisp))
(in-package :calculator)
(defun combine-expr (oprtr oprnd-1 lst)
  (list
   (list oprnd-1 oprtr (first lst))
   (rest lst)))
(defun combine-expr (oprtr oprnd-1 lst)
  (list
   (list oprtr oprnd-1 (first lst))
   (rest lst)))
