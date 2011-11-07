;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:simple-blog-asd
  (:use :cl :asdf))

(in-package :simple-blog-asd)

(defsystem simple-blog
    :name "simple-blog"
    :version "0.0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "simple-blog"
    :depends-on (:weblocks)
    :components ((:file "simple-blog")
		 (:module conf
		  :components ((:file "stores"))
		  :depends-on ("simple-blog"))
		 (:module src
		  :components ((:file "init-session")
                       (:file "models")
                       (:file "views"
                              :depends-on ("models"))
                       (:file "layout"
                              :depends-on ("models" "views")))
		  :depends-on ("simple-blog" conf))))
