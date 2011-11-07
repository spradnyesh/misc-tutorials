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