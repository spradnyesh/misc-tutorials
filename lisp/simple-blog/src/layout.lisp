(in-package :simple-blog)

(defun make-users-gridedit ()
  (make-instance 'gridedit
                 :name "users-grid"
                 :data-class 'user
                 :view 'user-table-view
                 :widget-prefix-fn (lambda (&rest args)
                                     (declare (ignore args))
                                     (with-html (:h1 "Users")))
                 :item-data-view 'user-data-view
                 :item-form-view 'user-form-view))
(defun make-posts-gridedit ()
  (make-instance 'gridedit
                 :name "posts-grid"
                 :data-class 'post
                 :view 'post-table-view
                 :widget-prefix-fn (lambda (&rest args)
                                     (declare (ignore args))
                                     (with-html (:h1 "Posts")))
                 :item-data-view 'post-data-view
                 :item-form-view 'post-form-view))
(defun make-admin-page ()
  (make-instance 'composite
                 :widgets
                 (list (make-users-gridedit)
                       (lambda ()
                         (with-html (:div (:hr :style "margin: 0px"))))
                       (make-posts-gridedit))))
