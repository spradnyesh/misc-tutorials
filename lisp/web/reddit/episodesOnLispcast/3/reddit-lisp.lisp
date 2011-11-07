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
