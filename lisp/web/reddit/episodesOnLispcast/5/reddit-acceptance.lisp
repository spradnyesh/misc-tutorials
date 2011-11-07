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
       (cl-ppcre:scan "0 points"
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
       (cl-ppcre:scan "0 points"
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
	"0 points"
	(sel:get-text 
	 (format nil "//div[a/@href='~A']/span[contains(@id, 'scorebox')]" url))))
      (remove-link (find title (get-links) :key #'title :test #'equal)))))

;; Acceptance Test
;; Story 6
;; User wants to change the score
;; he performs:
;;    browse to homepage
;;    chooses a link
;;    sees score
;;    sees "Up" link
;;    clicks "Up" link
;;    Sees score went up by one
(lisp-unit:define-test reddit-story-6
  (when (null (get-links))
    (add-link (create-link "Google" "http://www.google.com")))
  (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
    (sel:open "/")
    ;; scrape out the id
    (cl-ppcre:register-groups-bind ((#'s-utils:parse-integer-safely linkid))
	("(\\d+)scorebox"
	 (sel:get-attribute "//span[contains(@id, 'scorebox')]/@id"))
      ;; scrape out the score of the link with the id
      (cl-ppcre:register-groups-bind ((#'s-utils:parse-integer-safely score))
	  ("(\\d+) points"
	   (sel:get-text (format nil "//span[@id = '~ascorebox']" linkid)))
	;; check that we have the "Up" link
	(lisp-unit:assert-true (sel:is-element-present 
				(format nil "//a[@id = '~Aup' and . = 'Up']"
					linkid)))
	;; click it
	(sel:click (format nil "//a[@id = '~Aup' and . = 'Up']"
			   linkid))
	(sel:wait-for-page-to-load 5000)
	;; check that the score is still visible
	(lisp-unit:assert-true 
	 (sel:is-element-present 
	  (format nil "//span[@id = '~Ascorebox']" linkid)))
	;; scrape out the new score
	(cl-ppcre:register-groups-bind ((#'s-utils:parse-integer-safely newscore))
	    ("(\\d+) points" 
	     (sel:get-text (format nil "//span[@id = '~Ascorebox']" linkid)))
	  (lisp-unit:assert-equal newscore (1+ score)))))))

;; Acceptance Test
;; Story 7
;; User wants to change the score
;; he performs:
;;    browse to homepage
;;    chooses a link
;;    sees score
;;    sees "Down" link
;;    clicks "Down" link
;;    Sees score went down by one
(lisp-unit:define-test reddit-story-7
  (when (null (get-links))
    (add-link (create-link "Google" "http://www.google.com")))
  (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
    (sel:open "/")
    ;; scrape out the id
    (cl-ppcre:register-groups-bind ((#'s-utils:parse-integer-safely linkid))
	("(\\d+)scorebox"
	 (sel:get-attribute "//span[contains(@id, 'scorebox')]/@id"))
      ;; scrape out the score of the link with the id
      (cl-ppcre:register-groups-bind ((#'s-utils:parse-integer-safely score))
	  ("(\\d+) points"
	   (sel:get-text (format nil "//span[@id = '~ascorebox']" linkid)))
	
      
      
	;; check that we have the "Down" link
	(lisp-unit:assert-true (sel:is-element-present 
				(format nil "//a[@id = '~Adown' and . = 'Down']"
					linkid)))
	;; click it
	(sel:click (format nil "//a[@id = '~Adown' and . = 'Down']"
			   linkid))
	(sel:wait-for-page-to-load 5000)
	;; check that the score is still visible
	(lisp-unit:assert-true 
	 (sel:is-element-present 
	  (format nil "//span[@id = '~Ascorebox']" linkid)))
	;; scrape out the new score
	(cl-ppcre:register-groups-bind ((#'s-utils:parse-integer-safely newscore))
	    ("(\\d+) points" 
	     (sel:get-text (format nil "//span[@id = '~Ascorebox']" linkid)))
	  (lisp-unit:assert-equal newscore (1- score)))))))


(lisp-unit:run-tests 
 reddit-story-1
 reddit-story-2
 reddit-story-3
 reddit-story-4
 reddit-story-5
 reddit-story-6
 reddit-story-7)
