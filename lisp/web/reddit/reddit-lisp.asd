(in-package :cl-user)
(defpackage :reddit-lisp-asd
  (:use :cl :asdf))
(in-package :reddit-lisp-asd)
(asdf:defsystem :reddit-lisp
  :depends-on (:hunchentoot
               :cl-who
               :s-utils)
  :components ((:file "package")
               (:file "reddit-lisp"
                      :depends-on ("package"))))
