(in-package :reddit-lisp)

;; we'll use the hunchentoot library
#|(require 'hunchentoot)|#

;; we'll need the cl-who library
#|(require 'cl-who)|#

;; let's use the s-utils library
#|(require 's-utils)|#

;; and start the server
;(defvar *server* (hunchentoot:start-server :port 8080))
(defvar *server* (hunchentoot:start
                  (make-instance 'hunchentoot:acceptor :port 8080)))

;; we'll put the page to respond to /
(push (hunchentoot:create-regex-dispatcher "^/$" 'reddit-home)
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-regex-dispatcher "^/add$" 'add-form)
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-regex-dispatcher "^/save$" 'save-link)
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-regex-dispatcher "^/incpoint$" 'incpoint)
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-regex-dispatcher "^/decpoint$" 'decpoint)
      hunchentoot:*dispatch-table*)

;; we'll need a data structure to store our links in
(defclass link ()
  ((title
    :reader title
    :initarg :title)
   (url
    :reader url
    :initarg :url)
   ;; let's give a way to score the links
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

(defvar *last-id* 0)

(defun next-id ()
  (let ((id *last-id*))
    (incf *last-id*)
    id))

;; and a place to store them
;; we'll just store them as a list
(defvar *links* nil)

(defun display-links (str links)
  (cl-who:with-html-output (str)
    (dolist (l links)
      (cl-who:htm (:li (:a :href (url l)
                           (cl-who:str (title l)))
                       (cl-who:fmt "Created ~A ago. ~d points"
                                   (s-utils:format-duration
                                    (max 1 (- (get-universal-time)
                                              (timestamp l))))
                                   (score l))
                       (:a :href (format nil "/incpoint?id=~A" (id l))
                           "up")
                       (:a :href (format nil "/decpoint?id=~A" (id l))
                           "down")
                       )))))
;; the first page
(defun reddit-home () ;;hunchentoot handlers do not take any arguments
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in lisp!"))
     (:body
      (:h1 "A Reddit implementation in Common Lisp")
      (:h2 "Using sbcl and hunchentoot")
      (:a :href "/add" "Add new link")
      (:h3 "Highest Ranking Links")
      (:ol (display-links str (sort (copy-list *links*) #'> :key #'score)))
      (:h3 "Most Recent Links")
      (:ol (display-links str (sort (copy-list *links*) #'> :key #'timestamp)))))))
      
;; we need a way to add new links
(defun add-form (&optional message)
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in lisp! -- Add links"))
     (:body
      (:h1 "Add a new link")
      (when message (cl-who:htm (:div (cl-who:str message))))
      (:form :action "/save" :method "post"
             (:div "title:" (:input :type "text" :name "title"))
             (:div "url:" (:input :type "text" :name "url"))
             (:input :type "submit" :value "save"))))))

;; have to define the page to submit to
(defun save-link ()
  (let ((title (hunchentoot:parameter "title"))
        (url (hunchentoot:parameter "url")))
    ;; why don't we add some validation
    (cond
      ((or (null title)
           (zerop (length title)))
       (add-form "The title field is required"))
      ((or (null url)
           (zerop (length url)))
       (add-form "The url field is required"))
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
