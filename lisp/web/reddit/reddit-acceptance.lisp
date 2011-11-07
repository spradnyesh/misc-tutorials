(in-package :cl-user)

;; use lisp-unit for unit testing
(require 'lisp-unit)

;; use selenium for acceptance testing
(require 'selenium)

;; Acceptance Test
;; Story 1
;; User browses to homepage
;; he sees:
;;     "Common Lisp"
;;     "hunchentoot"
;;     "Reddit"
;;     "sbcl"
;;     "Add a link" link
;;     a bunch of links, each with a "Created .. ago" and a ".. points" string
(lisp-unit:define-test reddit-story-1
    (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
      (do-open "/")
      (lisp-unit:assert-true (is-text-present "Common Lisp"))))

(lisp-unit:run-tests reddit-story-1)
