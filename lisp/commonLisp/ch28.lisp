(defpackage :ch28
  (:use :cl))
(in-package :ch28)

(defun reverse1 (lst)
  (reverse2 lst '()))
(defun reverse2 (l1 l2)
  (if (null l1) l2
      (reverse2 (rest l1)
                (cons (first l1) l2))))
(defun reverse2 (l1 l2)
  (loop
       (when (null l1) (return l2))
             (push (pop l1) l2)))
(defun reverse3 (l)
  (let (l2)
    (loop
       (when (null l) (return l2))
       (push (pop l) l2))))
(defun reverse4 (l)
  (let (l2)
    (mapcar #'(lambda (e) (push e l2))
            l)
    l2)))
(defun reverse5 (l)
  (let (l2)
    (dolist (e l l2)
      (push e l2))))
