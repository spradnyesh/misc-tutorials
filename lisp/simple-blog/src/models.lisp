(in-package :simple-blog)

(defclass user ()
  ((id :documentation "this is automatically assigned by cl-prevalence when we persist the post object")
   (name :accessor user-name
         :initarg name
         :initform ""
         :type string)))

(defclass post ()
  ((id)
   (short-text :accessor post-short-text
               :initarg short-text
               :initform ""
               :type string
               :documentation "short text of the post")
   (text :accessor post-text
         :initarg text
         :initform ""
         :type string
         :documentation "long text of the post")
   (time :accessor post-time
         :initarg time
         :initform (get-universal-time)
         :documentation "time at which post was created")
   (author :accessor post-author
           :initarg author
           :initform ""
           :type user
           :documentation "author of blog post")))

(defgeneric post-author-id (post)
  (:method ((post post))
    (when (post-author post)
      (object-id (post-author post)))))
(defun all-users (&rest args)
  (declare (ignore args))
  (find-persistent-objects (class-store 'user) 'user))
