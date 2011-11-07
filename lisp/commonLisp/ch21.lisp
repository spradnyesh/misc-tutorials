(defpackage :ch21
  (:use :cl))
(in-package :ch21)

(defun scalar-plus (n vector)
  (mapcar #'(lambda (a) (+ n a)) vector))
(defun scalar-mul (n vector)
  (mapcar #'(lambda (a) (* n a)) vector))
(defun vector-product (vector1 vector2)
  (if (= 1 (length vector1))
      (list (scalar-mul (first vector1) vector2))
      (cons (scalar-mul (first vector1) vector2)
            (vector-product (rest vector1) vector2))))
