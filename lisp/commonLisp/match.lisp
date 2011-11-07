(defpackage :match
  (:use :cl))
(in-package :match)

(defun variablep (smbl)
  (char= (char smbl 0) #\?))
(defun match-element (e1 e2)
  (and (symbolp e1)
       (symbolp e2)
       (or
        (eql e1 e2)
        (or (variablep e1) (variablep e2)))))
