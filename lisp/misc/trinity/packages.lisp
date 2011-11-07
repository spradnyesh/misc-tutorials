;;;
;;; packages.lisp
;;;

(in-package :cl-user)

(defpackage :botlist-trinity
  (:use :cl :hunchentoot :cl-who)
  (:export :start-app
           :stop-app))

;;; End of File
