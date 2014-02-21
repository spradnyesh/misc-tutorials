(defpackage :codechef-system
  (:use :cl :asdf))

(in-package :codechef-system)

(defsystem codechef
  :components ((:module "src"
                        :components ((:file "package")
                                     (:file "drghts" :depends-on ("package"))))
               (:module "tests"
                        :components ((:file "package")
                                     (:file "drghts" :depends-on ("package")))
                        :depends-on ("src")))
  :depends-on (:fiveam))
