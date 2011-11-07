(defpackage :ch17
  (:use :cl))
(in-package "ch17")

(defun copy (lst)
  (check-type lst list)
  (if (null lst) '()
      (cons (first lst) (copy (rest lst)))))

(defun firstn (n l)
  "(firstn 3 '(1 2 (3 4) 5)) -> (1 2 (3 4))"
  (if (= n 0) '()
      (cons (first l) (firstn (1- n) (rest l)))))

(defun subst* (new old l)
  "(subst* 'x 'y '(a b y (x y) z y)) -> (A B X (X Y) Z X)"
  (check-type l list)
  (cond ((null l) '())
        ((equal (first l) old) (cons new (subst* new old (rest l))))
        (t (cons (first l) (subst* new old (rest l))))))
