(defpackage :ch23
  (:use :cl))
(in-package :ch23)

(defun pluslist-f (x y)
  `(+ ,x ,y))
(defmacro pluslist-m (x y)
  `(+ ,x ,y))
