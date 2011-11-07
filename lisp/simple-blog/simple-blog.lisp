
(defpackage #:simple-blog
  (:use :cl :weblocks
        :f-underscore :anaphora)
  (:import-from :hunchentoot #:header-in
		#:set-cookie #:set-cookie* #:cookie-in
		#:user-agent #:referer)
  (:documentation
   "A web application based on Weblocks."))

(in-package :simple-blog)

(export '(start-simple-blog stop-simple-blog))

;; A macro that generates a class or this webapp

(defwebapp simple-blog
    :prefix "/"
    :description "simple-blog: A new application"
    :init-user-session 'simple-blog::init-user-session
    :autostart nil                   ;; have to start the app manually
    :ignore-default-dependencies nil ;; accept the defaults
    :debug t
    )

;; Top level start & stop scripts

(defun start-simple-blog (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args)
  (start-webapp 'simple-blog))

(defun stop-simple-blog ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'simple-blog)
  (stop-weblocks))

