(in-package :cl-user)

;; use lisp-unit for unit testing
(require 'lisp-unit)

;; use selenium for acceptance testing
(require 'selenium)

;; use cl-ppcre for regular expressions
(require 'cl-ppcre)

;; Acceptance Test
;; Story 1
;; User browses to homepage
;; he sees:
;;    "Common Lisp"
;;    "hunchentoot"
;;    "Reddit"
;;    "sbcl"
;;    "Add a link" link
;;    a bunch of links, each with a "Created .. ago" and a ".. points" string
(lisp-unit:define-test reddit-story-1
  (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
    (sel:open "/login?username=eric&password=eric")
    (sel:open "/")
    (lisp-unit:assert-true (sel:is-text-present "Common Lisp"))
    (lisp-unit:assert-true (sel:is-text-present "hunchentoot"))
    (lisp-unit:assert-true (sel:is-text-present "sbcl"))
    (lisp-unit:assert-true (sel:is-text-present "Reddit"))
    ;; now to check the link
    (lisp-unit:assert-true (sel:is-element-present "//a[. = 'Add new link']"))
    (dolist (l (get-links))
      (let ((text (sel:get-text (format nil "//div[@id = '~Abox']" (id l)))))
	(lisp-unit:assert-true (cl-ppcre:scan "Created .+ ago" text))
	(lisp-unit:assert-true (cl-ppcre:scan "\\d+ points" text))
	(lisp-unit:assert-true (search (title l)
				       text))))))

;; Acceptance Test
;; Story 2
;; User wants to add a link
;; he performs:
;;    browse to homepage
;;    sees "Add a link" link
;;    clicks link
;;    sees "Add a new link"
;;    types in title
;;    types in url
;;    clicks "save"
;;    sees new link on homepage
(lisp-unit:define-test reddit-story-2
  (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
    (sel:open "/login?username=eric&password=eric")
    (sel:open "/")
    (lisp-unit:assert-true (sel:is-text-present "Common Lisp"))
    (lisp-unit:assert-true (sel:is-text-present "hunchentoot"))
    (lisp-unit:assert-true (sel:is-text-present "sbcl"))
    (lisp-unit:assert-true (sel:is-text-present "Reddit"))
    ;; now to check the link
    (lisp-unit:assert-true (sel:is-element-present "//a[. = 'Add new link']"))
    (dolist (l (get-links))
      (let ((text (sel:get-text (format nil "//div[@id = '~Abox']" (id l)))))
	(lisp-unit:assert-true (cl-ppcre:scan "Created .+ ago" text))
	(lisp-unit:assert-true (cl-ppcre:scan "\\d+ points" text))
	(lisp-unit:assert-true (search (title l)
				       text))))))

;; Acceptance Test
;; Story 3
;; User wants to add a link
;; he performs:
;;    browse to homepage
;;    sees "Add a link" link
;;    clicks link
;;    sees "Add a new link"
;;    clicks save (didn't type anything)
;;    sees "Add a new link"
;;    sees "The title is required"
;;    types in title
;;    types in url
;;    clicks "save"
;;    sees new link on homepage
(lisp-unit:define-test reddit-story-3
  (let ((title (format nil "title~A" (get-universal-time)))
	(url (format nil "http://www.url.com/~A" (get-universal-time))))
    (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
      (sel:open "/login?username=eric&password=eric")
      (sel:open "/")
      ;; now to check the link
      (lisp-unit:assert-true (sel:is-element-present "//a[. = 'Add new link']"))
      (sel:click "link=Add new link")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-true (sel:is-text-present "Add a new link"))
      (sel:click "//input[@value='save']")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-true (sel:is-text-present "Add a new link"))
      (lisp-unit:assert-true (sel:is-text-present "The title is required."))
      ;; first argument is the name of input element,
      ;; second is the text to input
      (sel:type "title" title)
      (sel:type "url" url)
      (sel:click "//input[@value='save']")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-equal title (sel:get-text (format nil "//a[@href='~A']" url)))
      (lisp-unit:assert-true 
       (cl-ppcre:scan "1 points"
		      (sel:get-text (format nil "//div[a/@href='~A']/span[contains(@id, 'scorebox')]" url))))
      (remove-link (find title (get-links) :key #'title :test #'equal)))))

;; Acceptance Test
;; Story 4
;; User wants to add a link
;; he performs:
;;    browse to homepage
;;    sees "Add a link" link
;;    clicks link
;;    sees "Add a new link"
;;    types in url
;;    clicks save 
;;    sees "Add a new link"
;;    sees "The title is required"
;;    types in title
;;    types in url
;;    clicks "save"
;;    sees new link on homepage
(lisp-unit:define-test reddit-story-4
  (let ((title (format nil "title~A" (get-universal-time)))
	(url (format nil "http://www.url.com/~A" (get-universal-time))))
    (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
      (sel:open "/login?username=eric&password=eric")
      (sel:open "/")
      ;; now to check the link
      (lisp-unit:assert-true (sel:is-element-present "//a[. = 'Add new link']"))
      (sel:click "link=Add new link")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-true (sel:is-text-present "Add a new link"))
      (sel:type "url" url)
      (sel:click "//input[@value='save']")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-true (sel:is-text-present "Add a new link"))
      (lisp-unit:assert-true (sel:is-text-present "The title is required."))
      ;; first argument is the name of input element,
      ;; second is the text to input
      (sel:type "title" title)
      (sel:type "url" url)
      (sel:click "//input[@value='save']")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-equal title (sel:get-text (format nil "//a[@href='~A']" url)))
      (lisp-unit:assert-true 
       (cl-ppcre:scan "1 points"
		      (sel:get-text (format nil "//div[a/@href='~A']/span[contains(@id, 'scorebox')]" url))))
      (remove-link (find title (get-links) :key #'title :test #'equal)))))


;; Acceptance Test
;; Story 5
;; User wants to add a link
;; he performs:
;;    browse to homepage
;;    sees "Add a link" link
;;    clicks link
;;    sees "Add a new link"
;;    types in title
;;    clicks save 
;;    sees "Add a new link"
;;    sees "The url is required"
;;    types in title
;;    types in url
;;    clicks "save"
;;    sees new link on homepage
(lisp-unit:define-test reddit-story-5
  (let ((title (format nil "title~A" (get-universal-time)))
	(url (format nil "http://www.url.com/~A" (get-universal-time))))
    (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
      (sel:open "/login?username=eric&password=eric")
      (sel:open "/")
      ;; now to check the link
      (lisp-unit:assert-true (sel:is-element-present "//a[. = 'Add new link']"))
      (sel:click "link=Add new link")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-true (sel:is-text-present "Add a new link"))
      (sel:type "title" title)
      (sel:click "//input[@value='save']")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-true (sel:is-text-present "Add a new link"))
      (lisp-unit:assert-true (sel:is-text-present "The url is required."))
      ;; first argument is the name of input element,
      ;; second is the text to input
      (sel:type "title" title)
      (sel:type "url" url)
      (sel:click "//input[@value='save']")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-equal title (sel:get-text (format nil "//a[@href='~A']" url)))
      (lisp-unit:assert-true 
       (cl-ppcre:scan 
	"1 points"
	(sel:get-text 
	 (format nil "//div[a/@href='~A']/span[contains(@id, 'scorebox')]" url))))
      (remove-link (find title (get-links) :key #'title :test #'equal)))))

;; Acceptance Test
;; Story 6
;; Our user wants to add a link
;; He's not logged in
;; he performs:
;;    setup -- logout
;;    browse to homepage
;;    click add a link
;;    sees "must log in"
;;    sees "Log in"
;;    sees "new user"
(lisp-unit:define-test reddit-story-6
  (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
      (sel:open "/logout")
      (sel:open "/")
      ;; now to check the link
      (lisp-unit:assert-true (sel:is-element-present "//a[. = 'Add new link']"))
      (sel:click "link=Add new link")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-true (sel:is-text-present "must log in"))
      (lisp-unit:assert-true (sel:is-text-present "Log in"))
      (lisp-unit:assert-true (sel:is-text-present "new user"))))


;; Acceptance Test
;; Story 7
;; Our user wants to score a link
;; He's not logged in
;; he performs:
;;    setup -- logout
;;    browse to homepage
;;    clicks on Up
;;    sees "must log in"
;;    sees "Log in"
;;    sees "new user"
(lisp-unit:define-test reddit-story-7
  (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
      (sel:open "/logout")
      (sel:open "/")
      ;; now to check the link
      (lisp-unit:assert-true (sel:is-element-present "//a[. = 'Up']"))
      (sel:click "link=Up")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-true (sel:is-text-present "must log in"))
      (lisp-unit:assert-true (sel:is-text-present "Log in"))
      (lisp-unit:assert-true (sel:is-text-present "new user"))))

;; Acceptance Test
;; Story 8
;; Our user wants to score a link
;; He's not logged in
;; he performs:
;;    setup -- logout
;;    browse to homepage
;;    clicks on Down
;;    sees "must log in"
;;    sees "Log in"
;;    sees "new user"
(lisp-unit:define-test reddit-story-8
  (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
      (sel:open "/logout")
      (sel:open "/")
      ;; now to check the link
      (lisp-unit:assert-true (sel:is-element-present "//a[. = 'Down']"))
      (sel:click "link=Down")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-true (sel:is-text-present "must log in"))
      (lisp-unit:assert-true (sel:is-text-present "Log in"))
      (lisp-unit:assert-true (sel:is-text-present "new user"))))


(lisp-unit:run-tests 
 reddit-story-1
 reddit-story-2
 reddit-story-3
 reddit-story-4
 reddit-story-5
 reddit-story-6
 reddit-story-7
 reddit-story-8)