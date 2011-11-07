(defpackage test
  (:use :cl))
(in-package test)

(defun div-gt-100 (x y)
  (and
   (numberp x)
   (numberp y)
   (or
    (= y 0)
    (> (/ x y) 100))))

(defun more-than-5 (obj)
  (and
   (or (stringp obj)
       (listp obj))
   (> (length obj) 5)))

(shadow 'cl:<=)
(defun <= (a b)
  (or
   (< a b)
   (= a b)))

(shadow 'cl:null)
(defun null (obj)
  (eql obj nil))

(defun non-empty-listp (obj)
  (and (listp obj) (> (length obj) 0)))

(shadow 'cl:not)
(defun not (obj)
  (null obj))

(defun div (x y)
  (if (= y 0) 9999999999
      (/ x y)))

(defun absval (x)
  (cond
    ((< x 0) (- x))
    ((= x 0) 0)
    ((> x 0) x)))

(defun sum (a b)
  (if (zerop a) b
      (1+ (sum (1- a) b))))
(defun product (a b)
  (cond
   ((or (zerop a) (zerop b)) 0)
   ((= a 1) b)
   (t (sum (product (1- a) b) b))))

(defun number-listp (l)
  (cond
   ((null l) t)
   ((not (numberp (first l))) nil)
   (t (number-listp (rest l)))))
(defun number-listp (l)
  (cond
   ((null l) t)
   ((numberp (first l)) (number-listp (rest l)))
   (t nil)))
(defun same-lengthp (l1 l2)
  (assert (listp l1) (l1) "1st argument should be a list, instead it is ~s" l1)
  (assert (listp l2) (l2) "1st argument should be a list, instead it is ~s" l2)
  (cond ((and (null l1) (null l2)) t)
        ((or (and (null l1) (not (null l2)))
             (and (null l2) (not (null l1))))
         nil)
        (t (same-lengthp (rest l1) (rest l2)))))
(defun same-lengthp (l1 l2)
  (assert (listp l1) (l1) "1st argument should be a list, instead it is ~s" l1)
  (assert (listp l2) (l2) "1st argument should be a list, instead it is ~s" l2)
  (cond ((null l1) (null l2))
        ((null l2) nil)
        (t (same-lengthp (rest l1) (rest l2)))))
(shadow 'cl:length)
(defun length (l)
  (assert (listp l) (l) "argument should be a list, instead it is ~s" l)
  (print l)
  (if (null l) 0
      (1+ (length (rest l)))))
(shadow 'cl:member)
(defun member (obj lst)
  (check-type lst list)
  (cond ((null lst) nil)
        ((eql obj (first lst)) lst)
        (t (member obj (rest lst)))))

(load "/home/pradyus/code/lisp/commonLisp/util.lisp")
(import '|util|:elementp)
(defun before (e1 e2 lst)
  (check-type e1 (satisfies elementp))
  (check-type e2 (satisfies elementp))
  (check-type lst list)
  (cond ((null lst) t)
        ((eql e1 (first lst)) t)
        ((eql e2 (first lst)) nil)
        (t (before e1 e2 (rest lst)))))

(shadow 'cl:count)
(defun count (e l)
  (check-type e (satisfies elementp))
  (check-type l list)
  (cond ((null l) 0)
        ((eql e (first l)) (1+ (count e (rest l))))
        (t (count e (rest l)))))

(defun equal-lelt (l1 l2)
  (check-type l1 list)
  (check-type l1 list)
  (cond ((null l1) (null l2))
        ((null l2) nil)
        ((eql (first l1) (first l2)) (equal-lelt (rest l1) (rest l2)))
        (t nil)))
(shadow 'cl:nth)
(defun nth (n l)
  (check-type l list)
  (if (= n 0) (first l)
      (nth (1- n) (rest l))))
(defun allbut (n l)
  (check-type l list)
  (if (= n 0) (rest l)
      (allbut (1- n) (rest l))))
(shadow 'cl:assoc)
(defun assoc (e al)
  (check-type al list)
  (if (eql e (first (first al))) (first al)
      (assoc e (rest al))))

(defun reverse1 (l)
  (check-type l list)
  (if (null l) '()
      (append (reverse1 (rest l)) (list (first l)))))
(defun reverse-help (l1 l2)
  "prepends reverse of l1 to l2"
  (check-type l1 list)
  (check-type l2 list)
  (if (null l1) l2
      (reverse-help (rest l1) (cons (first l1) l2))))
(defun reverse2 (l)
  (reverse-help l '()))

(deftype bag ()
  "Same as list; OR set w/ multiple entries of same element/value allowed"
  '(satisfies listp))
(defun makeset (b)
  "Returns a set containing just those elements of the input bag b."
  (check-type b bag)
  (cond ((null b) '())
        ((member (first b) (rest b))
         (makeset (rest b)))
        (t (cons (first b) (makeset (rest b))))))
