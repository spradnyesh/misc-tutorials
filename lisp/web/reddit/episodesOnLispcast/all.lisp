**********************************************************************
(in-package :cl-user)

;; we'll use the hunchentoot library
(require 'hunchentoot)

;; we'll need the cl-who library
(require 'cl-who)

;;let's use the s-utils library
(require 's-utils)

;; and start the server
(defvar *server* (hunchentoot:start-server :port 8080))

;; we'll put the page to respond to /
(push (hunchentoot:create-regex-dispatcher "^/$" 'reddit-home)
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-regex-dispatcher "^/add$" 'add-form)
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-regex-dispatcher "^/save$" 'save-link)
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-regex-dispatcher "^/incpoints$" 'incpoint)
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-regex-dispatcher "^/decpoints$" 'decpoint)
      hunchentoot:*dispatch-table*)


;; we'll need a data structure to store our links in
(defclass link ()
  ((title
    :reader title
    :initarg :title)
   (url
    :reader url
    :initarg :url)
   ;;let's give a way to score the links
   (score
    :accessor score
    :initform 0)
   (id
    :reader id
    :initform (next-id))
   ;; let's store the time it's created
   (timestamp
    :reader timestamp
    :initform (get-universal-time))
   ))

(defun next-id ()
  (let ((id *last-id*))
    (incf *last-id*)
    id))

(defvar *last-id* 0)

;; and a place to store them
;; we'll just store them as a list
(defvar *links* nil)

;; the first page
(defun reddit-home () ;; hunchentoot handlers take no arguments
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in Lisp!"))
     (:body
      (:h1 "A Reddit implementation in Common Lisp")
      (:h2 "Using sbcl and hunchentoot")
      (:a :href "/add" "Add new link")
      (:h3 "Highest Ranking Links")
      (:ol
       (dolist (l (sort (copy-list *links*) #'> :key #'score))
	 (cl-who:htm (:li (:a :href (url l)
			      (cl-who:str (title l)))
			  (cl-who:fmt "Created ~A ago. ~d points"
				      (s-utils:format-duration 
				       (max 1 (- (get-universal-time)
						 (timestamp l))))
				      (score l))
			  (:a :href (format nil "/incpoints?id=~A" (id l))
			      "Up")
			  (:a :href (format nil "/decpoints?id=~A" (id l))
			      "Down")))))
      (:h3 "Most Recent Links")
      (:ol
       (dolist (l (sort (copy-list *links*) #'> :key #'timestamp))
	 (cl-who:htm (:li (:a :href (url l)
			      (cl-who:str (title l)))
			  (cl-who:fmt "Created ~A ago. ~d points"
				      (s-utils:format-duration 
				       (max 1 (- (get-universal-time)
						 (timestamp l))))
				      (score l))
			  (:a :href (format nil "/incpoints?id=~A" (id l))
			      "Up")
			  (:a :href (format nil "/decpoints?id=~A" (id l))
			      "Down")))))
      
      
      )))
  )

;; we need a way to add a new link
(defun add-form (&optional message)
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in Lisp! -- Add link"))
     (:body
      (:h1 "Add a new link")
      (when message (cl-who:htm (:div (cl-who:str message))))
      (:form :action "/save" :method "post"
	     (:div "title:" (:input :type "text" :name "title"))
	     (:div "url:" (:input :type "text" :name "url"))
	     (:input :type "submit" :value "save"))))))

;; we have to define the page to submit to
(defun save-link ()
  (let ((title (hunchentoot:parameter "title"))
	(url (hunchentoot:parameter "url")))
    ;; why don't we add some validation
    (cond
      ((or (null title)
	   (zerop (length title)))
       (add-form "The title is required."))
      ((or (null url)
	   (zerop (length url)))
       (add-form "The url is required."))
      (t
       (push (make-instance 'link :title title :url url)
	     *links*)      
       (hunchentoot:redirect "/")))))

(defun incpoint ()
  (let* ((id (s-utils:parse-integer-safely
	      (hunchentoot:parameter "id")))
	 (link (find id *links* :key #'id)))
    (when link
      (incf (score link)))
    (hunchentoot:redirect "/")))

(defun decpoint ()
  (let* ((id (s-utils:parse-integer-safely
	      (hunchentoot:parameter "id")))
	 (link (find id *links* :key #'id)))
    (when link
      (decf (score link)))
    (hunchentoot:redirect "/")))
**********************************************************************
(in-package :cl-user)

;; we'll use the hunchentoot library
(require 'hunchentoot)

;; we'll need the cl-who library
(require 'cl-who)

;;let's use the s-utils library
(require 's-utils)

;; and start the server
(defvar *server* (hunchentoot:start-server :port 8080))

;; we'll put the page to respond to /
(push (hunchentoot:create-regex-dispatcher "^/$" 'reddit-home)
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-regex-dispatcher "^/add$" 'add-form)
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-regex-dispatcher "^/save$" 'save-link)
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-regex-dispatcher "^/incpoints$" 'incpoint)
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-regex-dispatcher "^/decpoints$" 'decpoint)
      hunchentoot:*dispatch-table*)


;; we'll need a data structure to store our links in
(defclass link ()
  ((title
    :reader title
    :initarg :title)
   (url
    :reader url
    :initarg :url)
   ;;let's give a way to score the links
   (score
    :accessor score
    :initform 0)
   (id
    :reader id
    :initform (next-id))
   ;; let's store the time it's created
   (timestamp
    :reader timestamp
    :initform (get-universal-time))
   ))

(defun next-id ()
  (let ((id *last-id*))
    (incf *last-id*)
    id))

(defvar *last-id* 0)

;; and a place to store them
;; we'll just store them as a list
(defvar *links* nil)

;; the first page
(defun reddit-home () ;; hunchentoot handlers take no arguments
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in Lisp!"))
     (:body
      (:h1 "A Reddit implementation in Common Lisp")
      (:h2 "Using sbcl and hunchentoot")
      (:a :href "/add" "Add new link")
      (:h3 "Highest Ranking Links")
      (:ol
       (dolist (l (sort (copy-list *links*) #'> :key #'score))
	 (cl-who:htm (:li (:div :id (format nil "~Abox" (id l)) 
				(:a :href (url l)
				    (cl-who:str (title l)))
				(:span :id (format nil "~Ascorebox" (id l)) 
				       (cl-who:fmt "Created ~A ago. ~d points"
						   (s-utils:format-duration 
						    (max 1 (- (get-universal-time)
							      (timestamp l))))
						   (score l)))
				(:a :id (format nil "~Aup" (id l)) 
				    :href (format nil "/incpoints?id=~A" (id l))
				    "Up")
				(:a :id (format nil "~Adown" (id l)) 
				    :href (format nil "/decpoints?id=~A" (id l))
				    "Down"))))))
      (:h3 "Most Recent Links")
      (:ol
       (dolist (l (sort (copy-list *links*) #'> :key #'timestamp))
	 (cl-who:htm (:li (:div :id (format nil "~Abox" (id l)) 
				(:a :href (url l)
				    (cl-who:str (title l)))
				(:span :id (format nil "~Ascorebox" (id l)) 
				       (cl-who:fmt "Created ~A ago. ~d points"
						   (s-utils:format-duration 
						    (max 1 (- (get-universal-time)
							      (timestamp l))))
						   (score l)))
				(:a :id (format nil "~Aup" (id l)) 
				    :href (format nil "/incpoints?id=~A" (id l))
				    "Up")
				(:a :id (format nil "~Adown" (id l)) 
				    :href (format nil "/decpoints?id=~A" (id l))
				    "Down"))))))
      
      
      )))
  )

;; we need a way to add a new link
(defun add-form (&optional message)
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in Lisp! -- Add link"))
     (:body
      (:h1 "Add a new link")
      (when message (cl-who:htm (:div (cl-who:str message))))
      (:form :action "/save" :method "post"
	     (:div "title:" (:input :type "text" :name "title"))
	     (:div "url:" (:input :type "text" :name "url"))
	     (:input :type "submit" :value "save"))))))

;; we have to define the page to submit to
(defun save-link ()
  (let ((title (hunchentoot:parameter "title"))
	(url (hunchentoot:parameter "url")))
    ;; why don't we add some validation
    (cond
      ((or (null title)
	   (zerop (length title)))
       (add-form "The title is required."))
      ((or (null url)
	   (zerop (length url)))
       (add-form "The url is required."))
      (t
       (push (make-instance 'link :title title :url url)
	     *links*)      
       (hunchentoot:redirect "/")))))

(defun incpoint ()
  (let* ((id (s-utils:parse-integer-safely
	      (hunchentoot:parameter "id")))
	 (link (find id *links* :key #'id)))
    (when link
      (incf (score link)))
    (hunchentoot:redirect "/")))

(defun decpoint ()
  (let* ((id (s-utils:parse-integer-safely
	      (hunchentoot:parameter "id")))
	 (link (find id *links* :key #'id)))
    (when link
      (decf (score link)))
    (hunchentoot:redirect "/")))
***********************************
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
    (dolist (l *links*)
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
  (let ((title (format nil "title~A" (get-universal-time)))
	(url (format nil "http://www.url.com/~A" (get-universal-time))))
    (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
      (sel:open "/")
      ;; now to check the link
      (lisp-unit:assert-true (sel:is-element-present "//a[. = 'Add new link']"))
      (sel:click "link=Add new link")
      (sel:wait-for-page-to-load 5000)
      (lisp-unit:assert-true (sel:is-text-present "Add a new link"))
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
      (setf *links* (delete title *links* :key #'title :test #'equal)))))

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
      (setf *links* (delete title *links* :key #'title :test #'equal)))))

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
      (setf *links* (delete title *links* :key #'title :test #'equal)))))

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
      (setf *links* (delete title *links* :key #'title :test #'equal)))))

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
  (let ((linkid 0)
	(score 0)
	(newscore 0))
    (when (null *links*)
      (push (make-instance 'link :title "Google" :url "http://www.google.com")
	    *links*))
    (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
      (sel:open "/")
      ;; scrape out the id
      (multiple-value-bind (match values)
	  (cl-ppcre:scan-to-strings 
	   "(\\d+)scorebox"
	   (sel:get-attribute "//span[contains(@id, 'scorebox')]/@id"))
	(setf linkid (elt values 0)))
      ;; scrape out the score of the link with the id
      (multiple-value-bind (match values)
	  (cl-ppcre:scan-to-strings
	   "(\\d+) points"
	   (sel:get-text (format nil "//span[@id = '~ascorebox']" linkid)))
	(setf score (s-utils:parse-integer-safely (elt values 0))))
      
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
      (multiple-value-bind (match values)
	  (cl-ppcre:scan-to-strings 
	   "(\\d+) points" 
	   (sel:get-text (format nil "//span[@id = '~Ascorebox']" linkid)))
	(setf newscore (s-utils:parse-integer-safely (elt values 0))))
      (lisp-unit:assert-equal newscore (1+ score)))))

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
  (let ((linkid 0)
	(score 0)
	(newscore 0))
    (when (null *links*)
      (push (make-instance 'link :title "Google" :url "http://www.google.com")
	    *links*))
    (selenium:with-selenium-session (*firefox "localhost" "http://localhost:8080")
      (sel:open "/")
      ;; scrape out the id
      (multiple-value-bind (match values)
	  (cl-ppcre:scan-to-strings 
	   "(\\d+)scorebox"
	   (sel:get-attribute "//span[contains(@id, 'scorebox')]/@id"))
	(setf linkid (elt values 0)))
      ;; scrape out the score of the link with the id
      (multiple-value-bind (match values)
	  (cl-ppcre:scan-to-strings
	   "(\\d+) points"
	   (sel:get-text (format nil "//span[@id = '~ascorebox']" linkid)))
	(setf score (s-utils:parse-integer-safely (elt values 0))))
      
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
      (multiple-value-bind (match values)
	  (cl-ppcre:scan-to-strings 
	   "(\\d+) points" 
	   (sel:get-text (format nil "//span[@id = '~Ascorebox']" linkid)))
	(setf newscore (s-utils:parse-integer-safely (elt values 0))))
      (lisp-unit:assert-equal newscore (1- score)))))


(lisp-unit:run-tests 
 reddit-story-1
 reddit-story-2
 reddit-story-3
 reddit-story-4
 reddit-story-5
 reddit-story-6
 reddit-story-7)
**********************************************************************
(in-package :cl-user)

;; we'll use the hunchentoot library
(require 'hunchentoot)

;; we'll need the cl-who library
(require 'cl-who)

;;let's use the s-utils library
(require 's-utils)

;; use the arnesi utilities library
(require 'arnesi)

;; cl-utilities library -- very useful
(require 'cl-utilities)

;; and start the server
(defvar *server* (hunchentoot:start
                  (make-instance 'hunchentoot:acceptor :port 8081)))


(defclass handler ()
  ((url
    :initarg :url
    :accessor url
    :type string)
   (handler
    :initarg :handler
    :accessor handler)
   (name
    :initarg :name
    :accessor name
    :type symbol)))

;; a place to store the handlers
;; store them by name, which is a symbol
(defvar *handlers* (make-hash-table))

;; now, we define our new dispatcher that uses the new data structure
(defun our-handler (request)
  (let ((handler (gethandler (hunchentoot:script-name request))))
    (when handler (handler handler))))

;; now we need to usurp power from the default dispatch table
(setf hunchentoot:*dispatch-table* (list 'our-handler))

;; let's use a macro to define a page
;; it will be pretty, not ugly
(defmacro defpage ((name url) &body body)
  (cl-utilities:once-only (url)
    `(setf (gethandler ',name)
      (make-instance 'handler
       :url ,url
       :name ',name
       :handler (lambda ()
		  ,@body)))))

(defmethod gethandler ((name symbol))
  (gethash name *handlers*))

(defmethod gethandler ((url string))
  (find url
	(gethandlers)
	:key #'url :test #'equal))

(defun gethandlers ()
  (arnesi:hash-table-values *handlers*))

(defun (setf gethandler) (handler name)
  (setf (gethash name *handlers*) handler))

(defmethod url ((name symbol))
  (arnesi:awhen (gethandler name)
    (url arnesi:it)))

(defmethod command ((name symbol) &rest args)
  (format nil "~A?~{~(~A~)=~A~^&~}" (url name) args))

;; we'll need a data structure to store our links in
(defclass link ()
  ((title
    :reader title
    :initarg :title)
   (url
    :reader url
    :initarg :url)
   ;;let's give a way to score the links
   (score
    :accessor score
    :initform 0)
   (id
    :reader id
    :initform (next-id))
   ;; let's store the time it's created
   (timestamp
    :reader timestamp
    :initform (get-universal-time))
   ))

(defun create-link (title url)
  (make-instance 'link :title title :url url))

(defun next-id ()
  (let ((id *last-id*))
    (incf *last-id*)
    id))

(defvar *last-id* 0)

(defmethod age ((link link))
  (- (get-universal-time)
     (timestamp link)))

;; and a place to store them
;; we'll just store them as a list
(defvar *links* nil)

(defun add-link (link)
  (push link *links*))

(defun get-links ()
  (copy-list *links*))

(defun get-link-by-id (id)
  (find id *links* :key #'id))

(defun get-links-sorted (key)
  (sort (get-links) #'> :key key))

(defun get-links-most-recent ()
  (get-links-sorted #'timestamp))

(defun get-links-highest-rank ()
  (get-links-sorted #'score))

;; the first page
;; our page definitions look different now
(defpage (reddit-home "/")
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in Lisp!"))
     (:body
      (:h1 "A Reddit implementation in Common Lisp")
      (:h2 "Using sbcl and hunchentoot")
      (:a :href (url 'add-form) "Add new link")
      (:h3 "Highest Ranking Links")
      (:ol
       (display-links str (get-links-highest-rank)))
      (:h3 "Most Recent Links")
      (:ol
       (display-links str (get-links-most-recent)))))))

(defun display-links (str links)
  (cl-who:with-html-output (str)
    (dolist (l links)
      (cl-who:htm (:li (display-link-box str l))))))

(defun display-link-box (str l)
  (cl-who:with-html-output (str)
    (:div :id (format nil "~Abox" (id l)) 
	  (display-link str l)
	  (display-link-age str l)
	  (display-link-score str l)
	  (display-link-score-control str l))))


(defun display-link (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm (:a :href (url l)
		    (cl-who:str (title l))))))

(defun display-link-age (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm 
     (:span :id (format nil "~Aagebox" (id l))
	    (cl-who:fmt "Created ~A ago."
			(s-utils:format-duration 
			 (max 1 (age l))))))))

(defun display-link-score (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:span :id (format nil "~Ascorebox" (id l))
	    (cl-who:fmt "~A points."
			(score l))))))

(defun display-link-score-control (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:span :id (format nil "~Ascorecontrol" (id l))
	    (:a :id (format nil "~Aup" (id l)) 
		:href (command 'incpoint "id" (id l))
		"Up")
	    " "
	    (:a :id (format nil "~Adown" (id l)) 
		:href (command 'decpoint "id" (id l))
		"Down")))))

;; we need a way to add a new link
;; the new page format
(defpage (add-form "/add")
  (add-form))

(defun add-form (&optional message)
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in Lisp! -- Add link"))
     (:body
      (:h1 "Add a new link")
      (when message (cl-who:htm (:div (cl-who:str message))))
      (:form :action (url 'save-link) :method "post"
	     (:div "title:" (:input :type "text" :name "title"))
	     (:div "url:" (:input :type "text" :name "url"))
	     (:input :type "submit" :value "save"))))))

;; we have to define the page to submit to
(defpage (save-link "/save")
  (let ((title (hunchentoot:parameter "title"))
	(url (hunchentoot:parameter "url")))
    ;; why don't we add some validation
    (cond
      ((zero-string title)
       (add-form "The title is required."))
      ((zero-string url)
       (add-form "The url is required."))
      (t
       (add-link (create-link title url))      
       (hunchentoot:redirect "/")))))

(defun zero-string (str)
  (or (null str)
      (zerop (length str))))

(defpage (incpoint "/incpoints")
  (let* ((id (get-integer-param "id"))
	 (link (get-link-by-id id)))
    (when link
      (incf (score link)))
    (hunchentoot:redirect "/")))

(defpage (decpoint "/decpoints")
  (let* ((id (get-integer-param "id"))
	 (link (get-link-by-id id)))
    (when link
      (decf (score link)))
    (hunchentoot:redirect "/")))

(defun get-integer-param (param)
  (s-utils:parse-integer-safely
   (hunchentoot:parameter param)))
**********************************************************************
(in-package :cl-user)

;; we'll use the hunchentoot library
(require 'hunchentoot)

;; we'll need the cl-who library
(require 'cl-who)

;;let's use the s-utils library
(require 's-utils)

;; use the arnesi utilities library
(require 'arnesi)

;; cl-utilities library -- very useful
(require 'cl-utilities)

(require 'clsql-mysql)

(clsql:locally-enable-sql-reader-syntax)

(setf clsql:*default-caching* nil)

;; and start the server
(defvar *server* (hunchentoot:start-server :port 8080))


(defmacro with-db ((database) &body body)
  `(clsql:with-database (,database '("localhost" "reddit" "reddit" "reddit")
			 :pool t :if-exists :old)
    ,@body))

(defclass handler ()
  ((url
    :initarg :url
    :accessor url
    :type string)
   (handler
    :initarg :handler
    :accessor handler)
   (name
    :initarg :name
    :accessor name
    :type symbol)))

;; a place to store the handlers
;; store them by name, which is a symbol
(defvar *handlers* (make-hash-table))

;; now, we define our new dispatcher that uses the new data structure
(defun our-handler (request)
  (let ((handler (gethandler (hunchentoot:script-name request))))
    (when handler (handler handler))))

;; now we need to usurp power from the default dispatch table
(setf hunchentoot:*dispatch-table* (list 'our-handler))

;; let's use a macro to define a page
;; it will be pretty, not ugly
(defmacro defpage ((name url) &body body)
  (cl-utilities:once-only (url)
    `(setf (gethandler ',name)
      (make-instance 'handler
       :url ,url
       :name ',name
       :handler (lambda ()
		  ,@body)))))

(defmethod gethandler ((name symbol))
  (gethash name *handlers*))

(defmethod gethandler ((url string))
  (find url
	(gethandlers)
	:key #'url :test #'equal))

(defun gethandlers ()
  (arnesi:hash-table-values *handlers*))

(defun (setf gethandler) (handler name)
  (setf (gethash name *handlers*) handler))

(defmethod url ((name symbol))
  (arnesi:awhen (gethandler name)
    (url arnesi:it)))

(defmethod command ((name symbol) &rest args)
  (format nil "~A?~{~(~A~)=~A~^&~}" (url name) args))

;; we'll need a data structure to store our links in
(clsql:def-view-class link ()
  ((title
    :reader title
    :initarg :title
    :type string)
   (url
    :reader url
    :initarg :url
    :type string)
   ;;let's give a way to score the links
   (score
    :accessor score
    :initform 0
    :type integer)
   (id
    :reader id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   ;; let's store the time it's created
   (timestamp
    :reader timestamp
    :initform (get-universal-time)
    :type integer
    :db-type "bigint")
   ))

(defmethod (setf score) :after (value (link link))
  (with-db (db)
    (clsql:update-record-from-slot link 'score :database db)))

(defun create-link (title url)
  (make-instance 'link :title title :url url))

(defmethod age ((link link))
  (- (get-universal-time)
     (timestamp link)))

(defun add-link (link)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:update-records-from-instance link :database db)
      (when (null (id link))
	(setf (slot-value link 'id)
	      (first
	       (first
		(clsql:query "SELECT MAX(ID) FROM LINK"))))))))


(defun get-links ()
  (with-db (db)
    (clsql:select 'link :flatp t :database db)))

(defun get-link-by-id (id)
  (with-db (db)
    (first
     (clsql:select 'link :flatp t :database db
		   :where [= [id] id]))))


(defun get-links-sorted (key)
  (sort (get-links) #'> :key key))

(defun get-links-most-recent ()
  (with-db (db)
    (clsql:select 'link :flatp t :database db
		  :order-by '(([timestamp] :desc)))))

(defun get-links-highest-rank ()
  (with-db (db)
    (clsql:select 'link :flatp t :database db
		  :order-by '(([score] :desc)))))

;; the first page
;; our page definitions look different now
(defpage (reddit-home "/")
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in Lisp!"))
     (:body
      (:h1 "A Reddit implementation in Common Lisp")
      (:h2 "Using sbcl and hunchentoot")
      (:a :href (url 'add-form) "Add new link")
      (:h3 "Highest Ranking Links")
      (:ol
       (display-links str (get-links-highest-rank)))
      (:h3 "Most Recent Links")
      (:ol
       (display-links str (get-links-most-recent)))))))

(defun display-links (str links)
  (cl-who:with-html-output (str)
    (dolist (l links)
      (cl-who:htm (:li (display-link-box str l))))))

(defun display-link-box (str l)
  (cl-who:with-html-output (str)
    (:div :id (format nil "~Abox" (id l)) 
	  (display-link str l)
	  (display-link-age str l)
	  (display-link-score str l)
	  (display-link-score-control str l))))


(defun display-link (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm (:a :href (url l)
		    (cl-who:str (title l))))))

(defun display-link-age (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm 
     (:span :id (format nil "~Aagebox" (id l))
	    (cl-who:fmt "Created ~A ago."
			(s-utils:format-duration 
			 (max 1 (age l))))))))

(defun display-link-score (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:span :id (format nil "~Ascorebox" (id l))
	    (cl-who:fmt "~A points."
			(score l))))))

(defun display-link-score-control (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:span :id (format nil "~Ascorecontrol" (id l))
	    (:a :id (format nil "~Aup" (id l)) 
		:href (command 'incpoint "id" (id l))
		"Up")
	    " "
	    (:a :id (format nil "~Adown" (id l)) 
		:href (command 'decpoint "id" (id l))
		"Down")))))

;; we need a way to add a new link
;; the new page format
(defpage (add-form "/add")
  (add-form))

(defun add-form (&optional message)
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in Lisp! -- Add link"))
     (:body
      (:h1 "Add a new link")
      (when message (cl-who:htm (:div (cl-who:str message))))
      (:form :action (url 'save-link) :method "post"
	     (:div "title:" (:input :type "text" :name "title"))
	     (:div "url:" (:input :type "text" :name "url"))
	     (:input :type "submit" :value "save"))))))

;; we have to define the page to submit to
(defpage (save-link "/save")
  (let ((title (hunchentoot:parameter "title"))
	(url (hunchentoot:parameter "url")))
    ;; why don't we add some validation
    (cond
      ((zero-string title)
       (add-form "The title is required."))
      ((zero-string url)
       (add-form "The url is required."))
      (t
       (add-link (create-link title url))      
       (hunchentoot:redirect "/")))))

(defun zero-string (str)
  (or (null str)
      (zerop (length str))))

(defpage (incpoint "/incpoints")
  (with-db (db)
    (clsql:with-transaction (:database db)
      (let* ((id (get-integer-param "id"))
	     (link (get-link-by-id id)))
	(when link
	  (incf (score link)))
	(hunchentoot:redirect "/")))))

(defpage (decpoint "/decpoints")
  (with-db (db)
    (clsql:with-transaction (:database db)
      (let* ((id (get-integer-param "id"))
	     (link (get-link-by-id id)))
	(when link
	  (decf (score link)))
	(hunchentoot:redirect "/")))))

(defun get-integer-param (param)
  (s-utils:parse-integer-safely
   (hunchentoot:parameter param)))

(clsql:locally-disable-sql-reader-syntax)
**********************************************************************
(in-package :cl-user)

;; we'll use the hunchentoot library
(require 'hunchentoot)

;; we'll need the cl-who library
(require 'cl-who)

;;let's use the s-utils library
(require 's-utils)

;; use the arnesi utilities library
(require 'arnesi)

;; cl-utilities library -- very useful
(require 'cl-utilities)

(require 'clsql-mysql)

(clsql:locally-enable-sql-reader-syntax)

(setf clsql:*default-caching* nil)

;; and start the server
(defvar *server* (hunchentoot:start-server :port 8080))


(defmacro with-db ((database) &body body)
  `(clsql:with-database (,database '("localhost" "reddit" "reddit" "reddit")
			 :pool t :if-exists :old)
    ,@body))

(defclass handler ()
  ((url
    :initarg :url
    :accessor url
    :type string)
   (handler
    :initarg :handler
    :accessor handler)
   (name
    :initarg :name
    :accessor name
    :type symbol)))

;; a place to store the handlers
;; store them by name, which is a symbol
(defvar *handlers* (make-hash-table))

;; now, we define our new dispatcher that uses the new data structure
(defun our-handler (request)
  (let ((handler (gethandler (hunchentoot:script-name request))))
    (when handler (handler handler))))

;; now we need to usurp power from the default dispatch table
(setf hunchentoot:*dispatch-table* (list 'our-handler))

;; let's use a macro to define a page
;; it will be pretty, not ugly
(defmacro defpage ((name url) &body body)
  (cl-utilities:once-only (url)
    `(setf (gethandler ',name)
      (make-instance 'handler
       :url ,url
       :name ',name
       :handler (lambda ()
		  ,@body)))))

(defmethod gethandler ((name symbol))
  (gethash name *handlers*))

(defmethod gethandler ((url string))
  (find url
	(gethandlers)
	:key #'url :test #'equal))

(defun gethandlers ()
  (arnesi:hash-table-values *handlers*))

(defun (setf gethandler) (handler name)
  (setf (gethash name *handlers*) handler))

(defmethod url ((name symbol))
  (arnesi:awhen (gethandler name)
    (url arnesi:it)))

(defmethod command ((name symbol) &rest args)
  (format nil "~A?~{~(~A~)=~A~^&~}" (url name) args))

;; we'll need a data structure to store our links in
(clsql:def-view-class link ()
  ((title
    :reader title
    :initarg :title
    :type string)
   (url
    :reader url
    :initarg :url
    :type string)
   (id
    :reader id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   ;; let's store the time it's created
   (timestamp
    :reader timestamp
    :initform (get-universal-time)
    :type integer
    :db-type "bigint")
   ))

(defmethod (setf score) :after (value (link link))
  (with-db (db)
    (clsql:update-record-from-slot link 'score :database db)))

(defun create-link (title url)
  (make-instance 'link :title title :url url))

(defmethod age ((link link))
  (- (get-universal-time)
     (timestamp link)))

(defun add-link (link)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:update-records-from-instance link :database db)
      (when (null (id link))
	(setf (slot-value link 'id)
	      (first
	       (first
		(clsql:query "SELECT MAX(ID) FROM LINK" :database db))))))))

(defun remove-link (link)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:delete-instance-records link))))

(defun get-links ()
  (with-db (db)
    (clsql:select 'link :flatp t :database db)))

(defun get-link-by-id (id)
  (with-db (db)
    (first
     (clsql:select 'link :flatp t :database db
		   :where [= [id] id]))))


(defun get-links-sorted (key)
  (sort (get-links) #'> :key key))

(defun get-links-most-recent ()
  (with-db (db)
    (clsql:select 'link :flatp t :database db
		  :order-by '(([timestamp] :desc)))))

(defun get-links-highest-rank ()
  (get-links-sorted #'score))

;; the first page
;; our page definitions look different now
(defpage (reddit-home "/")
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in Lisp!"))
     (:body
      (:h1 "A Reddit implementation in Common Lisp")
      (:h2 "Using sbcl and hunchentoot")
      (:a :href (url 'add-form) "Add new link")
      " | "
      (:a :href (url 'new-user-form) "Log in or create a new user")
      " | "
      (:a :href (url 'logout) "Log out")
      (:h3 "Highest Ranking Links")
      (:ol
       (display-links str (get-links-highest-rank)))
      (:h3 "Most Recent Links")
      (:ol
       (display-links str (get-links-most-recent)))))))

(defun display-links (str links)
  (cl-who:with-html-output (str)
    (dolist (l links)
      (cl-who:htm (:li (display-link-box str l))))))

(defun display-link-box (str l)
  (cl-who:with-html-output (str)
    (:div :id (format nil "~Abox" (id l)) 
	  (display-link str l)
	  (display-link-age str l)
	  (display-link-score str l)
	  (display-link-score-control str l))))


(defun display-link (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm (:a :href (url l)
		    (cl-who:str (title l))))))

(defun display-link-age (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm 
     (:span :id (format nil "~Aagebox" (id l))
	    (cl-who:fmt "Created ~A ago."
			(s-utils:format-duration 
			 (max 1 (age l))))))))

(defun display-link-score (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:span :id (format nil "~Ascorebox" (id l))
	    (cl-who:fmt "~A points."
			(score l))))))

(defun display-link-score-control (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:span :id (format nil "~Ascorecontrol" (id l))
	    (:a :id (format nil "~Aup" (id l)) 
		:href (command 'incpoint "id" (id l))
		"Up")
	    " "
	    (:a :id (format nil "~Adown" (id l)) 
		:href (command 'decpoint "id" (id l))
		"Down")))))

;; we need a way to add a new link
;; the new page format
(defpage (add-form "/add")
  (add-form))

(defun add-form (&optional message)
  (if (hunchentoot:session-value 'user)
   (cl-who:with-html-output-to-string (str)
     (:html
      (:head (:title "Reddit in Lisp! -- Add link"))
      (:body
       (:h1 "Add a new link")
       (when message (cl-who:htm (:div (cl-who:str message))))
       (:form :action (url 'save-link) :method "post"
	      (:div "title:" (:input :type "text" :name "title"))
	      (:div "url:" (:input :type "text" :name "url"))
	      (:input :type "submit" :value "save")))))
   (new-user-form "You must log in to add a link.")))

;; we have to define the page to submit to
(defpage (save-link "/save")
  (let ((title (hunchentoot:parameter "title"))
	(url (hunchentoot:parameter "url"))
	(user (hunchentoot:session-value 'user)))
    ;; why don't we add some validation
    (cond
      ((null user)
       (new-user-form "You must be logged in to add a link."))
      ((zero-string title)
       (add-form "The title is required."))
      ((zero-string url)
       (add-form "The url is required."))
      (t
       (add-rating (create-rating (id user)
				  (add-link (create-link title url))
				  1))      
       (hunchentoot:redirect "/")))))

(defun zero-string (str)
  (or (null str)
      (zerop (length str))))

(defpage (incpoint "/incpoints")
  (with-db (db)
    (clsql:with-transaction (:database db)
      (let* ((id (get-integer-param "id"))
	     (link (get-link-by-id id))
	     (user (hunchentoot:session-value 'user)))
	(cond
	  ((null user)
	   (new-user-form "You must log in or sign up to rate links."))
	  ((get-rating-by-userid-and-linkid (id user) (id link))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (link
	   (add-rating (create-rating (id user) (id link) 1))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (t
	   (hunchentoot:redirect (url 'reddit-home))))))))

(defpage (decpoint "/decpoints")
  (with-db (db)
    (clsql:with-transaction (:database db)
      (let* ((id (get-integer-param "id"))
	     (link (get-link-by-id id))
	     (user (hunchentoot:session-value 'user)))
	(cond
	  ((null user)
	   (new-user-form "You must log in or sign up to rate links."))
	  ((get-rating-by-userid-and-linkid (id user) (id link))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (link
	   (add-rating (create-rating (id user) (id link) -1))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (t
	   (hunchentoot:redirect (url 'reddit-home))))))))

(defun get-integer-param (param)
  (s-utils:parse-integer-safely
   (hunchentoot:parameter param)))

(clsql:def-view-class user ()
  ((id
    :accessor id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   (username
    :accessor username
    :initarg :username
    :type string)
   (password
    :accessor password
    :initarg :password
    :type string)))

(defun create-user (username password)
  (make-instance 'user :username username :password password))

(defun add-user (user)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:update-records-from-instance user :database db)
      (when (null (id user))
	(setf (slot-value user 'id)
	      (first
	       (clsql:query "SELECT MAX(ID) FROM USER" :flatp t :database db)))))))

(defun get-user-by-username (username)
  (with-db (db)
    (first
     (clsql:select 'user :flatp t :database db
		   :where [ = [ username ] username ]))))

(defun get-user-by-username-and-password (username password)
  (with-db (db)
    (first
     (clsql:select 'user :flatp t :database db
		   :where [ and [ = [ username ] username ]
		   [ = [ password ] password ] ]))))

(defpage (new-user-form "/new-user")
  (new-user-form))



(defun new-user-form (&optional message)
  (cl-who:with-html-output-to-string (str)
    (cl-who:htm
     (:html
      (:head (:title "New Reddit User"))
      (:body
       (when message (cl-who:htm (:div (cl-who:str message))))
       (:h1 "Log in")
       (:form :action (url 'login) :method "post"
	      (:div "username: " (:input :type "text" :name "username"))
	      (:div "password: " (:input :type "password" :name "password"))
	      (:input :type "submit" :value "log in"))
       (:h1 "Add new user")
       (:form :action (url 'add-user) :method "post"
	      (:div "username: " (:input :type "text" :name "username"))
	      (:div "password: " (:input :type "password" :name "password"))
	      (:div "repeat: " (:input :type "password" :name "password2"))
	      (:input :type "submit" :value "sign up")))))))

(defpage (add-user "/add-user")
  (let ((username (hunchentoot:parameter "username"))
	(password (hunchentoot:parameter "password"))
	(password2 (hunchentoot:parameter "password2")))
    (cond
      ((zero-string username)
       (new-user-form "You must provide a username."))
      ((get-user-by-username username)
       (new-user-form "The username you chose already exists."))
      ((zero-string password)
       (new-user-form "You must provide a password."))
      ((not (equal password password2))
       (new-user-form "The two passwords must match."))
      (t
       (add-user (create-user username password))
       (hunchentoot:redirect (url 'reddit-home))))))

(defpage (login "/login")
  (let* ((username (hunchentoot:parameter "username"))
	 (password (hunchentoot:parameter "password"))
	 (user (get-user-by-username-and-password username password)))
    (cond
      ((not (and username password))
       (new-user-form "Both the username and password are required to log in."))
      ((null user)
       (new-user-form "Username and password do not match our records."))
      (t
       (setf (hunchentoot:session-value 'user)
	     user)
       (hunchentoot:redirect (url 'reddit-home))))))

(defpage (logout "/logout")
  (setf (hunchentoot:session-value 'user) nil)
  (hunchentoot:redirect (url 'reddit-home)))

(clsql:def-view-class rating ()
  ((id
    :accessor id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   (raterid
    :accessor raterid
    :initarg :raterid
    :type integer)
   (linkid
    :accessor linkid
    :initarg :linkid
    :type integer)
   (size
    :accessor size
    :initform 1
    :initarg :size
    :type integer)
   (timestamp
    :accessor timestamp
    :initform (get-universal-time)
    :type integer
    :db-type "bigint")))

(defun create-rating (userid linkid size)
  (make-instance 'rating :raterid userid :linkid linkid :size size))

(defun add-rating (rating)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:update-records-from-instance rating :database db)
      (when (null (id rating))
	(setf (slot-value rating 'id)
	      (first
	       (clsql:query "SELECT MAX(ID) FROM RATING" :flatp t :database db)))))))

(defmethod score ((link link))
  (with-db (db)
    (loop for r in (clsql:select 'rating
				 :where [ = [ linkid ] (id link) ]
				 :flatp t :database db)
	 sum (size r))))

(defun get-rating-by-userid-and-linkid (userid linkid)
  (with-db (db)
    (first
     (clsql:select 'rating :where [ and [ = [ raterid ] userid ]
		   [ = [ linkid ] linkid ] ]
		   :flatp t
		   :database db))))



(clsql:locally-disable-sql-reader-syntax)
***********************************
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
**********************************************************************
(in-package :cl-user)

;; we'll use the hunchentoot library
(require 'hunchentoot)

;; we'll need the cl-who library
(require 'cl-who)

;;let's use the s-utils library
(require 's-utils)

;; use the arnesi utilities library
(require 'arnesi)

;; cl-utilities library -- very useful
(require 'cl-utilities)

(require 'clsql-mysql)

(clsql:locally-enable-sql-reader-syntax)

(setf clsql:*default-caching* nil)

;; and start the server
(defvar *server* (hunchentoot:start-server :port 8080))


(defmacro with-db ((database) &body body)
  `(clsql:with-database (,database '("localhost" "reddit" "reddit" "reddit")
			 :pool t :if-exists :old)
    ,@body))

(defclass handler ()
  ((url
    :initarg :url
    :accessor url
    :type string)
   (handler
    :initarg :handler
    :accessor handler)
   (name
    :initarg :name
    :accessor name
    :type symbol)))

;; a place to store the handlers
;; store them by name, which is a symbol
(defvar *handlers* (make-hash-table))

;; now, we define our new dispatcher that uses the new data structure
(defun our-handler (request)
  (let ((handler (gethandler (hunchentoot:script-name request))))
    (when handler (handler handler))))

;; now we need to usurp power from the default dispatch table
(setf hunchentoot:*dispatch-table* (list 'our-handler))

;; let's use a macro to define a page
;; it will be pretty, not ugly
(defmacro defpage ((name url) &body body)
  (cl-utilities:once-only (url)
    `(setf (gethandler ',name)
      (make-instance 'handler
       :url ,url
       :name ',name
       :handler (lambda ()
		  ,@body)))))

(defmethod gethandler ((name symbol))
  (gethash name *handlers*))

(defmethod gethandler ((url string))
  (find url
	(gethandlers)
	:key #'url :test #'equal))

(defun gethandlers ()
  (arnesi:hash-table-values *handlers*))

(defun (setf gethandler) (handler name)
  (setf (gethash name *handlers*) handler))

(defmethod url ((name symbol))
  (arnesi:awhen (gethandler name)
    (url arnesi:it)))

(defmethod command ((name symbol) &rest args)
  (format nil "~A?~{~(~A~)=~A~^&~}" (url name) args))

;; we'll need a data structure to store our links in
(clsql:def-view-class link ()
  ((title
    :reader title
    :initarg :title
    :type string)
   (url
    :reader url
    :initarg :url
    :type string)
   (id
    :reader id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   ;; let's store the time it's created
   (timestamp
    :reader timestamp
    :initform (get-universal-time)
    :type integer
    :db-type "bigint")
   ))

(defmethod (setf score) :after (value (link link))
  (with-db (db)
    (clsql:update-record-from-slot link 'score :database db)))

(defun create-link (title url)
  (make-instance 'link :title title :url url))

(defmethod age ((link link))
  (- (get-universal-time)
     (timestamp link)))

(defun add-link (link)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:update-records-from-instance link :database db)
      (when (null (id link))
	(setf (slot-value link 'id)
	      (first
	       (first
		(clsql:query "SELECT MAX(ID) FROM LINK" :database db))))))))

(defun remove-link (link)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:delete-instance-records link))))

(defun get-links ()
  (with-db (db)
    (clsql:select 'link :flatp t :database db)))

(defun get-link-by-id (id)
  (with-db (db)
    (first
     (clsql:select 'link :flatp t :database db
		   :where [= [id] id]))))


(defun get-links-sorted (key)
  (sort (get-links) #'> :key key))

(defun get-links-most-recent ()
  (with-db (db)
    (clsql:select 'link :flatp t :database db
		  :order-by '(([timestamp] :desc)))))

(defun get-links-highest-rank ()
  (get-links-sorted #'score))

;; the first page
;; our page definitions look different now
(defpage (reddit-home "/")
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in Lisp!"))
     (:body
      (:h1 "A Reddit implementation in Common Lisp")
      (:h2 "Using sbcl and hunchentoot")
      (:a :href (url 'add-form) "Add new link")
      " | "
      (:a :href (url 'new-user-form) "Log in or create a new user")
      " | "
      (:a :href (url 'logout) "Log out")
      (:h3 "Highest Ranking Links")
      (:ol
       (display-links str (get-links-highest-rank)))
      (:h3 "Most Recent Links")
      (:ol
       (display-links str (get-links-most-recent)))))))

(defun display-links (str links)
  (cl-who:with-html-output (str)
    (dolist (l links)
      (cl-who:htm (:li (display-link-box str l))))))

(defun display-link-box (str l)
  (cl-who:with-html-output (str)
    (:div :id (format nil "~Abox" (id l)) 
	  (display-link str l)
	  (display-link-age str l)
	  (display-link-score str l)
	  (display-link-score-control str l))))


(defun display-link (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm (:a :href (url l)
		    (cl-who:str (title l))))))

(defun display-link-age (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm 
     (:span :id (format nil "~Aagebox" (id l))
	    (cl-who:fmt "Created ~A ago."
			(s-utils:format-duration 
			 (max 1 (age l))))))))

(defun display-link-score (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:span :id (format nil "~Ascorebox" (id l))
	    (cl-who:fmt "~A points."
			(score l))))))

(defun display-link-score-control (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:span :id (format nil "~Ascorecontrol" (id l))
	    (:a :id (format nil "~Aup" (id l)) 
		:href (command 'incpoint "id" (id l))
		"Up")
	    " "
	    (:a :id (format nil "~Adown" (id l)) 
		:href (command 'decpoint "id" (id l))
		"Down")))))

;; we need a way to add a new link
;; the new page format
(defpage (add-form "/add")
  (add-form))

(defun add-form (&optional message)
  (if (hunchentoot:session-value 'user)
   (cl-who:with-html-output-to-string (str)
     (:html
      (:head (:title "Reddit in Lisp! -- Add link"))
      (:body
       (:h1 "Add a new link")
       (when message (cl-who:htm (:div (cl-who:str message))))
       (:form :action (url 'save-link) :method "post"
	      (:div "title:" (:input :type "text" :name "title"))
	      (:div "url:" (:input :type "text" :name "url"))
	      (:input :type "submit" :value "save")))))
   (new-user-form "You must log in to add a link.")))

;; we have to define the page to submit to
(defpage (save-link "/save")
  (let ((title (hunchentoot:parameter "title"))
	(url (hunchentoot:parameter "url"))
	(user (hunchentoot:session-value 'user)))
    ;; why don't we add some validation
    (cond
      ((null user)
       (new-user-form "You must be logged in to add a link."))
      ((zero-string title)
       (add-form "The title is required."))
      ((zero-string url)
       (add-form "The url is required."))
      (t
       (add-rating (create-rating (id user)
				  (add-link (create-link title url))
				  1))      
       (hunchentoot:redirect "/")))))

(defun zero-string (str)
  (or (null str)
      (zerop (length str))))

(defpage (incpoint "/incpoints")
  (with-db (db)
    (clsql:with-transaction (:database db)
      (let* ((id (get-integer-param "id"))
	     (link (get-link-by-id id))
	     (user (hunchentoot:session-value 'user)))
	(cond
	  ((null user)
	   (new-user-form "You must log in or sign up to rate links."))
	  ((get-rating-by-userid-and-linkid (id user) (id link))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (link
	   (add-rating (create-rating (id user) (id link) 1))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (t
	   (hunchentoot:redirect (url 'reddit-home))))))))

(defpage (decpoint "/decpoints")
  (with-db (db)
    (clsql:with-transaction (:database db)
      (let* ((id (get-integer-param "id"))
	     (link (get-link-by-id id))
	     (user (hunchentoot:session-value 'user)))
	(cond
	  ((null user)
	   (new-user-form "You must log in or sign up to rate links."))
	  ((get-rating-by-userid-and-linkid (id user) (id link))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (link
	   (add-rating (create-rating (id user) (id link) -1))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (t
	   (hunchentoot:redirect (url 'reddit-home))))))))

(defun get-integer-param (param)
  (s-utils:parse-integer-safely
   (hunchentoot:parameter param)))

(clsql:def-view-class user ()
  ((id
    :accessor id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   (username
    :accessor username
    :initarg :username
    :type string)
   (password
    :accessor password
    :initarg :password
    :type string)))

(defun create-user (username password)
  (make-instance 'user :username username :password password))

(defun add-user (user)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:update-records-from-instance user :database db)
      (when (null (id user))
	(setf (slot-value user 'id)
	      (first
	       (clsql:query "SELECT MAX(ID) FROM USER" :flatp t :database db)))))))

(defun get-user-by-username (username)
  (with-db (db)
    (first
     (clsql:select 'user :flatp t :database db
		   :where [ = [ username ] username ]))))

(defun get-user-by-username-and-password (username password)
  (with-db (db)
    (first
     (clsql:select 'user :flatp t :database db
		   :where [ and [ = [ username ] username ]
		   [ = [ password ] password ] ]))))

(defpage (new-user-form "/new-user")
  (new-user-form))



(defun new-user-form (&optional message)
  (cl-who:with-html-output-to-string (str)
    (cl-who:htm
     (:html
      (:head (:title "New Reddit User"))
      (:body
       (when message (cl-who:htm (:div (cl-who:str message))))
       (:h1 "Log in")
       (:form :action (url 'login) :method "post"
	      (:div "username: " (:input :type "text" :name "username"))
	      (:div "password: " (:input :type "password" :name "password"))
	      (:input :type "submit" :value "log in"))
       (:h1 "Add new user")
       (:form :action (url 'add-user) :method "post"
	      (:div "username: " (:input :type "text" :name "username"))
	      (:div "password: " (:input :type "password" :name "password"))
	      (:div "repeat: " (:input :type "password" :name "password2"))
	      (:input :type "submit" :value "sign up")))))))

(defpage (add-user "/add-user")
  (let ((username (hunchentoot:parameter "username"))
	(password (hunchentoot:parameter "password"))
	(password2 (hunchentoot:parameter "password2")))
    (cond
      ((zero-string username)
       (new-user-form "You must provide a username."))
      ((get-user-by-username username)
       (new-user-form "The username you chose already exists."))
      ((zero-string password)
       (new-user-form "You must provide a password."))
      ((not (equal password password2))
       (new-user-form "The two passwords must match."))
      (t
       (add-user (create-user username password))
       (hunchentoot:redirect (url 'reddit-home))))))

(defpage (login "/login")
  (let* ((username (hunchentoot:parameter "username"))
	 (password (hunchentoot:parameter "password"))
	 (user (get-user-by-username-and-password username password)))
    (cond
      ((not (and username password))
       (new-user-form "Both the username and password are required to log in."))
      ((null user)
       (new-user-form "Username and password do not match our records."))
      (t
       (setf (hunchentoot:session-value 'user)
	     user)
       (hunchentoot:redirect (url 'reddit-home))))))

(defpage (logout "/logout")
  (setf (hunchentoot:session-value 'user) nil)
  (hunchentoot:redirect (url 'reddit-home)))

(clsql:def-view-class rating ()
  ((id
    :accessor id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   (raterid
    :accessor raterid
    :initarg :raterid
    :type integer)
   (linkid
    :accessor linkid
    :initarg :linkid
    :type integer)
   (size
    :accessor size
    :initform 1
    :initarg :size
    :type integer)
   (timestamp
    :accessor timestamp
    :initform (get-universal-time)
    :type integer
    :db-type "bigint")))

(defun create-rating (userid linkid size)
  (make-instance 'rating :raterid userid :linkid linkid :size size))

(defun add-rating (rating)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:update-records-from-instance rating :database db)
      (when (null (id rating))
	(setf (slot-value rating 'id)
	      (first
	       (clsql:query "SELECT MAX(ID) FROM RATING" :flatp t :database db)))))))

(defmethod score ((link link))
  (with-db (db)
    (loop for r in (clsql:select 'rating
				 :where [ = [ linkid ] (id link) ]
				 :flatp t :database db)
	 sum (size r))))

(defun get-rating-by-userid-and-linkid (userid linkid)
  (with-db (db)
    (first
     (clsql:select 'rating :where [ and [ = [ raterid ] userid ]
		   [ = [ linkid ] linkid ] ]
		   :flatp t
		   :database db))))



(clsql:locally-disable-sql-reader-syntax)
***********************************
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
**********************************************************************
(in-package :cl-user)

;; we'll use the hunchentoot library
(require 'hunchentoot)

;; we'll need the cl-who library
(require 'cl-who)

;;let's use the s-utils library
(require 's-utils)

;; use the arnesi utilities library
(require 'arnesi)

;; cl-utilities library -- very useful
(require 'cl-utilities)

(require 'clsql-mysql)

(clsql:locally-enable-sql-reader-syntax)

(setf clsql:*default-caching* nil)

;; and start the server
(defvar *server* (hunchentoot:start-server :port 8080))


(defmacro with-db (&body body)
  (cl-utilities:with-unique-names (database)
   `(clsql:with-database (,database '("localhost" "reddit" "reddit" "reddit")
				    :pool t :if-exists :old)
      (clsql-sys:with-default-database (,database)
	,@body))))

(defclass handler ()
  ((url
    :initarg :url
    :accessor url
    :type string)
   (url-fun
    :initarg :url-fun
    :accessor url-fun
    :type function)
   (handler
    :initarg :handler
    :accessor handler)
   (name
    :initarg :name
    :accessor name
    :type symbol)))

;; a place to store the handlers
;; store them by name, which is a symbol
(defvar *handlers* (make-hash-table))

;; now, we define our new dispatcher that uses the new data structure
(defun our-handler (request)
  (let ((handler (gethandler (hunchentoot:script-name request))))
    (when handler (handler handler))))

;; now we need to usurp power from the default dispatch table
(setf hunchentoot:*dispatch-table* (list 'our-handler))

(defvar *+reddit-stream+*)
(declaim (special *+reddit-stream+*))

(defmacro with-page-output (&body body)
  `(cl-who:with-html-output-to-string (*+reddit-stream+*)
     ,@body))

;; let's use a macro to define a page
;; it will be pretty, not ugly
(defmacro defpage ((name url) (&rest args) &body body)
  (cl-utilities:once-only (url)
    `(setf (gethandler ',name)
      (make-instance 'handler
       :url ,url
       :url-fun ,(if (not args)
		     `(lambda (handler &rest args)
			(url handler))
		     `(lambda (handler &key ,@args)
			(format nil
				,(format nil "~~A?~{~(~A~)=~~A~^&~}"
					 args)
				(url handler)
				,@args)))
       :name ',name
       :handler (lambda ()
		  (with-page-output
		    ,@body))))))

(defmacro defsnippet (name args &body body)
  `(defun ,name ,args
     (cl-who:with-html-output (*+reddit-stream+*)
       ,@body)))

(defmethod gethandler ((name symbol))
  (gethash name *handlers*))

(defmethod gethandler ((url string))
  (find url
	(gethandlers)
	:key #'url :test #'equal))

(defun gethandlers ()
  (arnesi:hash-table-values *handlers*))

(defun (setf gethandler) (handler name)
  (setf (gethash name *handlers*) handler))

(defmethod url ((name symbol))
  (arnesi:awhen (gethandler name)
    (url arnesi:it)))

(defmethod command ((name symbol) &rest args)
  (arnesi:awhen (gethandler name)
    (apply (url-fun arnesi:it) arnesi:it args)))

;; we'll need a data structure to store our links in
(clsql:def-view-class link ()
  ((title
    :reader title
    :initarg :title
    :type string)
   (url
    :reader url
    :initarg :url
    :type string)
   (id
    :reader id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   ;; let's store the time it's created
   (timestamp
    :reader timestamp
    :initform (get-universal-time)
    :type integer
    :db-type "bigint")
   ))

(defmethod (setf score) :after (value (link link))
  (with-db
    (clsql:update-record-from-slot link 'score)))

(defun create-link (title url)
  (make-instance 'link :title title :url url))

(defmethod age ((link link))
  (- (get-universal-time)
     (timestamp link)))

(defun add-link (link)
  (with-db
    (clsql:with-transaction ()
      (clsql:update-records-from-instance link)
      (when (null (id link))
	(setf (slot-value link 'id)
	      (first
	       (first
		(clsql:query "SELECT MAX(ID) FROM LINK"))))))))

(defun remove-link (link)
  (with-db
    (clsql:with-transaction ()
      (clsql:delete-instance-records link))))

(defun get-links ()
  (with-db 
    (clsql:select 'link :flatp t)))

(defun get-link-by-id (id)
  (with-db
    (first
     (clsql:select 'link :flatp t
		   :where [= [id] id]))))


(defun get-links-sorted (key)
  (sort (get-links) #'> :key key))

(defun get-links-most-recent ()
  (with-db
    (clsql:select 'link :flatp t
		  :order-by '(([timestamp] :desc)))))

(defun get-links-highest-rank ()
  (get-links-sorted #'score))

;; the first page
;; our page definitions look different now
(defpage (reddit-home "/") ()  
  (:html
   (:head (:title "Reddit in Lisp!"))
   (:body
    (:h1 "A Reddit implementation in Common Lisp")
    (:h2 "Using sbcl and hunchentoot")
    (:a :href (url 'add-form) "Add new link")
    " | "
    (:a :href (url 'new-user-form) "Log in or create a new user")
    " | "
    (:a :href (url 'logout) "Log out")
    (:h3 "Highest Ranking Links")
    (:ol
     (display-links (get-links-highest-rank)))
    (:h3 "Most Recent Links")
    (:ol
     (display-links (get-links-most-recent))))))

(defsnippet display-links (links)
  (dolist (l links)
    (cl-who:htm (:li (display-link-box l)))))

(defsnippet display-link-box (l)
  (:div :id (format nil "~Abox" (id l)) 
	(display-link l)
	(display-link-age l)
	(display-link-score l)
	(display-link-score-control l)))


(defsnippet display-link (l)
  (:a :href (url l)
      (cl-who:str (title l))))

(defsnippet display-link-age (l)
  (:span :id (format nil "~Aagebox" (id l))
	 (cl-who:fmt "Created ~A ago."
		     (s-utils:format-duration 
		      (max 1 (age l))))))

(defsnippet display-link-score (l)
  (:span :id (format nil "~Ascorebox" (id l))
	 (cl-who:fmt "~A points."
		     (score l))))

(defsnippet display-link-score-control (l)
  (:span :id (format nil "~Ascorecontrol" (id l))
	 (:a :id (format nil "~Aup" (id l)) 
	     :href (command 'incpoint :id (id l))
	     "Up")
	 " "
	 (:a :id (format nil "~Adown" (id l)) 
	     :href (command 'decpoint :id (id l))
	     "Down")))

;; we need a way to add a new link
;; the new page format
(defpage (add-form "/add") ()
  (add-form))

(defsnippet add-form (&optional message)
  (if (hunchentoot:session-value 'user)
      (cl-who:htm
       (:html
	(:head (:title "Reddit in Lisp! -- Add link"))
	(:body
	 (:h1 "Add a new link")
	 (when message (cl-who:htm (:div (cl-who:str message))))
	 (:form :action (url 'save-link) :method "post"
		(:div "title:" (:input :type "text" :name "title"))
		(:div "url:" (:input :type "text" :name "url"))
		(:input :type "submit" :value "save")))))
      (new-user-form "You must log in to add a link.")))

;; we have to define the page to submit to
(defpage (save-link "/save") ()
  (let ((title (hunchentoot:parameter "title"))
	(url (hunchentoot:parameter "url"))
	(user (hunchentoot:session-value 'user)))
    ;; why don't we add some validation
    (cond
      ((null user)
       (new-user-form "You must be logged in to add a link."))
      ((zero-string title)
       (add-form "The title is required."))
      ((zero-string url)
       (add-form "The url is required."))
      (t
       (add-rating (create-rating (id user)
				  (add-link (create-link title url))
				  1))      
       (hunchentoot:redirect "/")))))

(defun zero-string (str)
  (or (null str)
      (zerop (length str))))

(defpage (incpoint "/incpoints") (id)
  (with-db
    (clsql:with-transaction ()
      (let* ((id (get-integer-param "id"))
	     (link (get-link-by-id id))
	     (user (hunchentoot:session-value 'user)))
	(cond
	  ((null user)
	   (new-user-form "You must log in or sign up to rate links."))
	  ((get-rating-by-userid-and-linkid (id user) (id link))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (link
	   (add-rating (create-rating (id user) (id link) 1))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (t
	   (hunchentoot:redirect (url 'reddit-home))))))))

(defpage (decpoint "/decpoints") (id)
  (with-db
    (clsql:with-transaction ()
      (let* ((id (get-integer-param "id"))
	     (link (get-link-by-id id))
	     (user (hunchentoot:session-value 'user)))
	(cond
	  ((null user)
	   (new-user-form "You must log in or sign up to rate links."))
	  ((get-rating-by-userid-and-linkid (id user) (id link))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (link
	   (add-rating (create-rating (id user) (id link) -1))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (t
	   (hunchentoot:redirect (url 'reddit-home))))))))

(defun get-integer-param (param)
  (s-utils:parse-integer-safely
   (hunchentoot:parameter param)))

(clsql:def-view-class user ()
  ((id
    :accessor id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   (username
    :accessor username
    :initarg :username
    :type string)
   (password
    :accessor password
    :initarg :password
    :type string)))

(defun create-user (username password)
  (make-instance 'user :username username :password password))

(defun add-user (user)
  (with-db
    (clsql:with-transaction ()
      (clsql:update-records-from-instance user)
      (when (null (id user))
	(setf (slot-value user 'id)
	      (first
	       (clsql:query "SELECT MAX(ID) FROM USER" :flatp t)))))))

(defun get-user-by-username (username)
  (with-db
    (first
     (clsql:select 'user :flatp t
		   :where [ = [ username ] username ]))))

(defun get-user-by-username-and-password (username password)
  (with-db
    (first
     (clsql:select 'user :flatp t
		   :where [ and [ = [ username ] username ]
		   [ = [ password ] password ] ]))))

(defpage (new-user-form "/new-user") ()
  (new-user-form))



(defsnippet new-user-form (&optional message)
  (:html
   (:head (:title "New Reddit User"))
   (:body
    (when message (cl-who:htm (:div (cl-who:str message))))
    (:h1 "Log in")
    (:form :action (url 'login) :method "post"
	   (:div "username: " (:input :type "text" :name "username"))
	   (:div "password: " (:input :type "password" :name "password"))
	   (:input :type "submit" :value "log in"))
    (:h1 "Add new user")
    (:form :action (url 'add-user) :method "post"
	   (:div "username: " (:input :type "text" :name "username"))
	   (:div "password: " (:input :type "password" :name "password"))
	   (:div "repeat: " (:input :type "password" :name "password2"))
	   (:input :type "submit" :value "sign up")))))

(defpage (add-user "/add-user") ()
  (let ((username (hunchentoot:parameter "username"))
	(password (hunchentoot:parameter "password"))
	(password2 (hunchentoot:parameter "password2")))
    (cond
      ((zero-string username)
       (new-user-form "You must provide a username."))
      ((get-user-by-username username)
       (new-user-form "The username you chose already exists."))
      ((zero-string password)
       (new-user-form "You must provide a password."))
      ((not (equal password password2))
       (new-user-form "The two passwords must match."))
      (t
       (add-user (create-user username password))
       (hunchentoot:redirect (url 'reddit-home))))))

(defpage (login "/login") ()
  (let* ((username (hunchentoot:parameter "username"))
	 (password (hunchentoot:parameter "password"))
	 (user (get-user-by-username-and-password username password)))
    (cond
      ((not (and username password))
       (new-user-form "Both the username and password are required to log in."))
      ((null user)
       (new-user-form "Username and password do not match our records."))
      (t
       (setf (hunchentoot:session-value 'user)
	     user)
       (hunchentoot:redirect (url 'reddit-home))))))

(defpage (logout "/logout") ()
  (setf (hunchentoot:session-value 'user) nil)
  (hunchentoot:redirect (url 'reddit-home)))

(clsql:def-view-class rating ()
  ((id
    :accessor id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   (raterid
    :accessor raterid
    :initarg :raterid
    :type integer)
   (linkid
    :accessor linkid
    :initarg :linkid
    :type integer)
   (size
    :accessor size
    :initform 1
    :initarg :size
    :type integer)
   (timestamp
    :accessor timestamp
    :initform (get-universal-time)
    :type integer
    :db-type "bigint")))

(defun create-rating (userid linkid size)
  (make-instance 'rating :raterid userid :linkid linkid :size size))

(defun add-rating (rating)
  (with-db
    (clsql:with-transaction ()
      (clsql:update-records-from-instance rating)
      (when (null (id rating))
	(setf (slot-value rating 'id)
	      (first
	       (clsql:query "SELECT MAX(ID) FROM RATING" :flatp t)))))))

(defmethod score ((link link))
  (with-db
    (loop for r in (clsql:select 'rating
				 :where [ = [ linkid ] (id link) ]
				 :flatp t)
	 sum (size r))))

(defun get-rating-by-userid-and-linkid (userid linkid)
  (with-db
    (first
     (clsql:select 'rating :where [ and [ = [ raterid ] userid ]
		   [ = [ linkid ] linkid ] ]
		   :flatp t))))



(clsql:locally-disable-sql-reader-syntax)
**********************************************************************
(in-package :cl-user)

;; we'll use the hunchentoot library
(require 'hunchentoot)

;; we'll need the cl-who library
(require 'cl-who)

;;let's use the s-utils library
(require 's-utils)

;; use the arnesi utilities library
(require 'arnesi)

;; cl-utilities library -- very useful
(require 'cl-utilities)

(require 'clsql-mysql)

(require 'cl-cont)

(clsql:locally-enable-sql-reader-syntax)

(setf clsql:*default-caching* nil)

;; and start the server
(defvar *server* (hunchentoot:start-server :port 8080))


(defmacro with-db ((database) &body body)
  `(clsql:with-database (,database '("localhost" "reddit" "reddit" "reddit")
			 :pool t :if-exists :old)
    ,@body))

(defclass handler ()
  ((url
    :initarg :url
    :accessor url
    :type string)
   (handler
    :initarg :handler
    :accessor handler)
   (name
    :initarg :name
    :accessor name
    :type symbol)))

;; a place to store the handlers
;; store them by name, which is a symbol
(defvar *handlers* (make-hash-table))

;; now, we define our new dispatcher that uses the new data structure
(defun our-handler (request)
  (let ((handler (gethandler (hunchentoot:script-name request))))
    (when handler (handler handler))))

;; now we need to usurp power from the default dispatch table
(setf hunchentoot:*dispatch-table* (list 'our-handler))

;; let's use a macro to define a page
;; it will be pretty, not ugly
(defmacro defpage ((name url) &body body)
  (cl-utilities:once-only (url)
    `(setf (gethandler ',name)
      (make-instance 'handler
       :url ,url
       :name ',name
       :handler (cl-cont:lambda/cc ()
		  ,@body)))))

(defmethod gethandler ((name symbol))
  (gethash name *handlers*))

(defmethod gethandler ((url string))
  (find url
	(gethandlers)
	:key #'url :test #'equal))

(defun gethandlers ()
  (arnesi:hash-table-values *handlers*))

(defun (setf gethandler) (handler name)
  (setf (gethash name *handlers*) handler))

(defmethod url ((name symbol))
  (arnesi:awhen (gethandler name)
    (url arnesi:it)))

(defmethod command ((name symbol) &rest args)
  (format nil "~A?~{~(~A~)=~A~^&~}" (url name) args))

;; we'll need a data structure to store our links in
(clsql:def-view-class link ()
  ((title
    :reader title
    :initarg :title
    :type string)
   (url
    :reader url
    :initarg :url
    :type string)
   (id
    :reader id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   ;; let's store the time it's created
   (timestamp
    :reader timestamp
    :initform (get-universal-time)
    :type integer
    :db-type "bigint")
   ))

(defmethod (setf score) :after (value (link link))
  (with-db (db)
    (clsql:update-record-from-slot link 'score :database db)))

(defun create-link (title url)
  (make-instance 'link :title title :url url))

(defmethod age ((link link))
  (- (get-universal-time)
     (timestamp link)))

(defun add-link (link)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:update-records-from-instance link :database db)
      (when (null (id link))
	(setf (slot-value link 'id)
	      (first
	       (first
		(clsql:query "SELECT MAX(ID) FROM LINK" :database db))))))))

(defun remove-link (link)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:delete-instance-records link))))

(defun get-links ()
  (with-db (db)
    (clsql:select 'link :flatp t :database db)))

(defun get-link-by-id (id)
  (with-db (db)
    (first
     (clsql:select 'link :flatp t :database db
		   :where [= [id] id]))))


(defun get-links-sorted (key)
  (sort (get-links) #'> :key key))

(defun get-links-most-recent ()
  (with-db (db)
    (clsql:select 'link :flatp t :database db
		  :order-by '(([timestamp] :desc)))))

(defun get-links-highest-rank ()
  (get-links-sorted #'score))

(defun continuations-hash ()
  (or (hunchentoot:session-value 'continuations)
      (setf (hunchentoot:session-value 'continuations)
	    (make-hash-table :test 'equal))))

(defun save-continuation (K)
  (let ((key (format nil "~A" (gensym "cont"))))
    (setf (gethash key (continuations-hash)) K)
    (command 'continuation "cont" key)))

(defun retrieve-continuation (key)
  (gethash key (continuations-hash)))

(defpage (continuation "/continuation")
  (let ((K (retrieve-continuation (hunchentoot:get-parameter "cont"))))
    (if (typep K 'function)
	(funcall K)
      (hunchentoot:redirect (url 'reddit-home)))))

;; the first page
;; our page definitions look different now
(defpage (reddit-home "/")
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in Lisp!"))
     (:body
      (:h1 "A Reddit implementation in Common Lisp")
      (:h2 "Using sbcl and hunchentoot")
      (:a :href (url 'add-form) "Add new link")
      " | "
      (:a :href (url 'new-user-form) "Log in or create a new user")
      " | "
      (:a :href (url 'logout) "Log out")
      (:h3 "Highest Ranking Links")
      (:ol
       (display-links str (get-links-highest-rank)))
      (:h3 "Most Recent Links")
      (:ol
       (display-links str (get-links-most-recent)))))))

(defun display-links (str links)
  (cl-who:with-html-output (str)
    (dolist (l links)
      (cl-who:htm (:li (display-link-box str l))))))

(defun display-link-box (str l)
  (cl-who:with-html-output (str)
    (:div :id (format nil "~Abox" (id l)) 
	  (display-link str l)
	  (display-link-age str l)
	  (display-link-score str l)
	  (display-link-score-control str l))))


(defun display-link (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm (:a :href (url l)
		    (cl-who:str (title l))))))

(defun display-link-age (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm 
     (:span :id (format nil "~Aagebox" (id l))
	    (cl-who:fmt "Created ~A ago."
			(s-utils:format-duration 
			 (max 1 (age l))))))))

(defun display-link-score (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:span :id (format nil "~Ascorebox" (id l))
	    (cl-who:fmt "~A points."
			(score l))))))

(defun display-link-score-control (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:span :id (format nil "~Ascorecontrol" (id l))
	    (:a :id (format nil "~Aup" (id l)) 
		:href (command 'incpoint "id" (id l))
		"Up")
	    " "
	    (:a :id (format nil "~Adown" (id l)) 
		:href (command 'decpoint "id" (id l))
		"Down")))))

;; we need a way to add a new link
;; the new page format
(defpage (add-form "/add")
  (let ((title (hunchentoot:parameter "title"))
	(url (hunchentoot:parameter "url")))
    (loop while (or (not title) (not url))
	  do (add-form "The title and url must not be blank."))
    (save-page)))

(cl-cont:defun/cc add-form (&optional message)
  (let ((message ""))
    (loop while (not (hunchentoot:session-value 'user))
	  do (new-user-form message)
	  do (let* ((username (hunchentoot:parameter "username"))
		    (password (hunchentoot:parameter "password"))
		    (password2 (hunchentoot:parameter "password2"))
		    (submit (hunchentoot:parameter "submit")))
	       (if (equal "log in" submit)
		   (let ((user (get-user-by-username-and-password username password)))
		     (cond
		      ((not (and username password))
		       (setf message "Both the username and password are required to log in."))
		      ((null user)
		       (setf message "Username and password do not match our records."))
		      (t
		       (setf (hunchentoot:session-value 'user)
			     user))))
		 (cond
		  ((zero-string username)
		   (setf message "You must provide a username."))
		  ((get-user-by-username username)
		   (setf message "The username you chose already exists."))
		  ((zero-string password)
		   (setf message "You must provide a password."))
		  ((not (equal password password2))
		   (setf message "The two passwords must match."))
		  (t
		   (add-user (create-user username password))
		   (setf (hunchentoot:session-value 'user)
			 (get-user-by-username-and-password username password))))))))
  (cl-cont:let/cc 
   K
   (cl-who:with-html-output-to-string (str)
				      (:html
				       (:head (:title "Reddit in Lisp! -- Add link"))
				       (:body
					(:h1 "Add a new link")
					(when message (cl-who:htm (:div (cl-who:str message))))
					(:form :action (save-continuation K) :method "post"
					       (:div "title:" (:input :type "text" :name "title"))
					       (:div "url:" (:input :type "text" :name "url"))
					       (:input :type "submit" :value "save")))))))

(defpage (save-page "/save-page")
  (save-page))
;; we have to define the page to submit to
(cl-cont:defun/cc save-page ()
		  
  (let ((title (hunchentoot:parameter "title"))
	(url (hunchentoot:parameter "url"))
	(user (hunchentoot:session-value 'user)))
    ;; why don't we add some validation
    (cond
      ((null user)
       (new-user-form "You must be logged in to add a link."))
      ((zero-string title)
       (add-form "The title is required."))
      ((zero-string url)
       (add-form "The url is required."))
      (t
       (add-rating (create-rating (id user)
				  (add-link (create-link title url))
				  1))      
       (hunchentoot:redirect "/")))))

(defun zero-string (str)
  (or (null str)
      (zerop (length str))))

(defpage (incpoint "/incpoints")
  (with-db (db)
    (clsql:with-transaction (:database db)
      (let* ((id (get-integer-param "id"))
	     (link (get-link-by-id id))
	     (user (hunchentoot:session-value 'user)))
	(cond
	  ((null user)
	   (new-user-form "You must log in or sign up to rate links."))
	  ((get-rating-by-userid-and-linkid (id user) (id link))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (link
	   (add-rating (create-rating (id user) (id link) 1))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (t
	   (hunchentoot:redirect (url 'reddit-home))))))))

(defpage (decpoint "/decpoints")
  (with-db (db)
    (clsql:with-transaction (:database db)
      (let* ((id (get-integer-param "id"))
	     (link (get-link-by-id id))
	     (user (hunchentoot:session-value 'user)))
	(cond
	  ((null user)
	   (new-user-form "You must log in or sign up to rate links."))
	  ((get-rating-by-userid-and-linkid (id user) (id link))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (link
	   (add-rating (create-rating (id user) (id link) -1))
	   (hunchentoot:redirect (url 'reddit-home)))
	  (t
	   (hunchentoot:redirect (url 'reddit-home))))))))

(defun get-integer-param (param)
  (s-utils:parse-integer-safely
   (hunchentoot:parameter param)))

(clsql:def-view-class user ()
  ((id
    :accessor id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   (username
    :accessor username
    :initarg :username
    :type string)
   (password
    :accessor password
    :initarg :password
    :type string)))

(defun create-user (username password)
  (make-instance 'user :username username :password password))

(defun add-user (user)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:update-records-from-instance user :database db)
      (when (null (id user))
	(setf (slot-value user 'id)
	      (first
	       (clsql:query "SELECT MAX(ID) FROM USER" :flatp t :database db)))))))

(defun get-user-by-username (username)
  (with-db (db)
    (first
     (clsql:select 'user :flatp t :database db
		   :where [ = [ username ] username ]))))

(defun get-user-by-username-and-password (username password)
  (with-db (db)
    (first
     (clsql:select 'user :flatp t :database db
		   :where [ and [ = [ username ] username ]
		   [ = [ password ] password ] ]))))

(defpage (new-user-form "/new-user")
  
  (let ((done nil)
	(message ""))
    (loop while (not done)
	  do (new-user-form message)
	  do (let* ((username (hunchentoot:parameter "username"))
		    (password (hunchentoot:parameter "password"))
		    (password2 (hunchentoot:parameter "password2"))
		    (submit (hunchentoot:parameter "submit")))
	       (if (equal "log in" submit)
		   (let ((user (get-user-by-username-and-password username password)))
		     (cond
		      ((not (and username password))
		       (setf message "Both the username and password are required to log in."))
		      ((null user)
		       (setf message "Username and password do not match our records."))
		      (t
		       (setf (hunchentoot:session-value 'user)
			     user)
		       (setf done t))))
		 (cond
		  ((zero-string username)
		   (setf message "You must provide a username."))
		  ((get-user-by-username username)
		   (setf message "The username you chose already exists."))
		  ((zero-string password)
		   (setf message "You must provide a password."))
		  ((not (equal password password2))
		   (setf message "The two passwords must match."))
		  (t
		   (add-user (create-user username password))
		   (setf done t)))))
	  )
    )
  (hunchentoot:redirect (url 'reddit-home)))



(cl-cont:defun/cc new-user-form (&optional message)
  (cl-cont:let/cc 
   K
   (cl-who:with-html-output-to-string (str)
				      (cl-who:htm
				       (:html
					(:head (:title "New Reddit User"))
					(:body
					 (when message (cl-who:htm (:div (cl-who:str message))))
					 
					 (:form :action (save-continuation K) :method "post"
						(:h1 "Log in")
						(:div "username: " (:input :type "text" :name "username"))
						(:div "password: " (:input :type "password" :name "password"))
						(:input :type "submit" :name "submit" :value "log in")
						(:h1 "Add new user")
						(:div "username: " (:input :type "text" :name "username"))
						(:div "password: " (:input :type "password" :name "password"))
						(:div "repeat: " (:input :type "password" :name "password2"))
						(:input :type "submit" :name "submit" :value "sign up"))))))))

(defpage (add-user "/add-user")
  (let ((username (hunchentoot:parameter "username"))
	(password (hunchentoot:parameter "password"))
	(password2 (hunchentoot:parameter "password2")))
    (cond
      ((zero-string username)
       (new-user-form "You must provide a username."))
      ((get-user-by-username username)
       (new-user-form "The username you chose already exists."))
      ((zero-string password)
       (new-user-form "You must provide a password."))
      ((not (equal password password2))
       (new-user-form "The two passwords must match."))
      (t
       (add-user (create-user username password))
       (hunchentoot:redirect (url 'reddit-home))))))

(defpage (login "/login")
  (let* ((username (hunchentoot:parameter "username"))
	 (password (hunchentoot:parameter "password"))
	 (user (get-user-by-username-and-password username password)))
    (cond
      ((not (and username password))
       (new-user-form "Both the username and password are required to log in."))
      ((null user)
       (new-user-form "Username and password do not match our records."))
      (t
       (setf (hunchentoot:session-value 'user)
	     user)
       (hunchentoot:redirect (url 'reddit-home))))))

(defpage (logout "/logout")
  (setf (hunchentoot:session-value 'user) nil)
  (hunchentoot:redirect (url 'reddit-home)))

(clsql:def-view-class rating ()
  ((id
    :accessor id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   (raterid
    :accessor raterid
    :initarg :raterid
    :type integer)
   (linkid
    :accessor linkid
    :initarg :linkid
    :type integer)
   (size
    :accessor size
    :initform 1
    :initarg :size
    :type integer)
   (timestamp
    :accessor timestamp
    :initform (get-universal-time)
    :type integer
    :db-type "bigint")))

(defun create-rating (userid linkid size)
  (make-instance 'rating :raterid userid :linkid linkid :size size))

(defun add-rating (rating)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:update-records-from-instance rating :database db)
      (when (null (id rating))
	(setf (slot-value rating 'id)
	      (first
	       (clsql:query "SELECT MAX(ID) FROM RATING" :flatp t :database db)))))))

(defmethod score ((link link))
  (with-db (db)
    (loop for r in (clsql:select 'rating
				 :where [ = [ linkid ] (id link) ]
				 :flatp t :database db)
	 sum (size r))))

(defun get-rating-by-userid-and-linkid (userid linkid)
  (with-db (db)
    (first
     (clsql:select 'rating :where [ and [ = [ raterid ] userid ]
		   [ = [ linkid ] linkid ] ]
		   :flatp t
		   :database db))))



(clsql:locally-disable-sql-reader-syntax)
**********************************************************************
