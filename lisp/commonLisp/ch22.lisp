(defpackage :ch22
  (:use :cl))
(in-package :ch22)

(defun append2 (lst1 lst2)
  (check-type lst1 list)
  (check-type lst2 list)
  (if (null lst1) lst2
      (cons (first lst1) (append2 (rest lst1) lst2))))
(defun append3 (&rest lists);??? how the hell does this work???
  (cond ((null lists) '())
        ((null (rest lists)) (first lists))
        (t (append3
            (first lists)
            (eval `(append3 ,@(mapcar
                              #'(lambda (l) (list 'quote l))
                              (rest lists))))))))
(defun append4 (&rest lists)
  (cond ((null lists) '())
        ((null (rest lists)) (first lists))
        (t (append2 (first lists)
                    (apply #'append4 (rest lists))))))
(defun vector-sum (&rest vectors);??? ask this in group
  (apply #'mapcar #'+ vectors))

(defun max-length (&rest lists)
  (apply #'max
         (mapcar #'(lambda (lst) (funcall #'length lst))
                 lists)))
(defun mapcar-ext (fn &rest lists)
  (let ((max-length (max-length lists)))
    max-length))

