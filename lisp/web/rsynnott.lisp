(defpackage :demoserv2
  (:use :hunchentoot
        :cl-who
        :cl)
  (:export
   :start-server))

(in-package :demoserv2)

(setq *dispatch-table* (list (create-regex-dispatcher "^/login$" 'login)))
                           

(defparameter *users* '(("mags" "icecream" (:scary-hair :evil))
                        ("annie" "mrpugwash" (:ann-widdicombe))
                        ("tony" "gwbush" (:evil :boring :scary-wife))
                        ("john" "grey" (:boring))))
                      

(defvar *our-mutex* (hunchentoot-mp:make-lock "our-lock"))
(defvar *events* '())

; Add event to log
(defun add-event (user text)
  (hunchentoot-mp:with-lock (*our-mutex*)
    (push `(,user ,text) *events*)))

; Does this user have access to these roles?
(defun has-access (user-details required-roles)
  (reduce #'(lambda (a b) (and a b))
          (mapcar #'(lambda (role) (find role (second user-details)))
                  required-roles)))

; Get user details from session
(defmacro with-user-details (user-details &body body)
  `(let ((,user-details (session-value :user)))
     ,@body))

; Display body if the user is allowed access it
(defmacro display-with-roles (user-details required-roles &body body)
  `(if (and user-details (has-access ,user-details ,required-roles))
       ,@body))

; Template
(defmacro with-template (title user-details &body body)
  `(with-html-output-to-string (*standard-output*)
     (:html
      (:head (:title (fmt "Hunchentoot demo - ~a" ,title)))
      (:body (:h1 "Hunchentoot Demo")
             (:div (:a :href "/" "Menu") " - "
                   (:a :href "/events" "Events")
                   (if user-details
                       (htm " - " (:a :href "/logout" "Logout"))))
             ,@body))))

; Defines a normal page with basic infrastructure
(defmacro defpage (name url required-roles user-details &body body)
  `(progn
     (defun ,name ()
       (with-user-details ,user-details
         (if (or (not ,required-roles)
                 (has-access ,user-details ,required-roles))
             (progn ,@body)
             "You're not allowed view this page")))
     (push (create-regex-dispatcher ,(format nil "^/~a$" url) ',name)
           *dispatch-table*)))

; Defines an event page
(defmacro defevent (name url required-roles text)
  `(defpage ,name ,url ,required-roles user-details
            (add-event (first user-details) ,text)
            (with-template ,text user-details
                           (htm (:h3 "Event Registered") (str ,text)))))

; Index
(defpage index-page "" nil user-details
         (with-template "Index" user-details
                        (:div
                         (if (not user-details)
                             (htm (:form :action "/login" :method "post"
                                         "Username:" (:input :type "text" :name "username")
                                         "Password:" (:input :type "password" :name "password")
                                         (:input :type "submit" :value "submit"))))
                         (htm (:ul
                               (display-with-roles user-details '(:scary-hair)
                                                   (htm (:li (:a :href "/impose-poll-tax" "Impose Poll Tax"))))
                               (display-with-roles user-details '(:evil)
                                                   (htm (:li (:a :href "/have-a-war" "Have a Nice War"))))
                               (display-with-roles user-details '(:scary-hair :evil)
                                                   (htm (:li (:a :href "/ice-cream" "Invent Soft Ice-cream"))))
                               (display-with-roles user-details '(:boring)
                                                   (htm (:li (:a :href "/eat-peas" "Eat Peas"))))
                               (display-with-roles user-details '(:scary-wife)
                                                   (htm (:li (:a :href "/flats" "Have Flat Investment Scandal"))))
                               (display-with-roles user-details '(:ann-widdicombe)
                                                   (htm (:li (:a :href "/celeb-fat" "Go on Celebrity Fat Farm")))))))))

; Events
(defevent impose-poll-tax "impose-poll-tax" '(:scary-hair) "Poll tax imposed!")
(defevent celeb-fat "celeb-fat" '(:ann-widdicombe) "Went to Celebrity Fat Farm!")
(defevent have-a-war "have-a-war" '(:evil) "Had a nice war. I do like those.")
(defevent ice-cream "ice-cream" '(:evil :scary-hair) "Invented soft ice-cream.")
(defevent eat-peas "eat-peas" '(:boring) "Peas are nice, dear!")
(defevent flats "flats" '(:scary-wife) "Buy flat through fraudster.")

(defpage logout "logout" nil user-details
         (delete-session-value :user)
         (redirect "/"))

; Events listing
(defpage events "events" nil user-details
         (let* ((user (parameter "user"))
                (our-events (if user
                                (remove-if-not #'(lambda (a) (equal user a))
                                               (hunchentoot-mp:with-lock (*our-mutex*)
                                                 *events*)
                                               :key #'first)
                                (hunchentoot-mp:with-lock (*our-mutex*)
                                  *events*))))
           (with-template "Events" user-details
                          (htm (:h3 "Events")
                               (:ul
                                (dolist (i our-events)
                                  (htm (:li (:a :href (format nil "/events?user=~a" (first i)) (str (first i))) " - "
                                            (str (second i))))))))))

(defun login ()
  (let ((username (parameter "username"))
        (password (parameter "password")))
    (let ((user-details (find username *users* :test #'equal :key #'first)))
      (if user-details
          (cond ((equal (second user-details) password)
                 (start-session)
                 (setf (session-value :user) (list (first user-details)
                                                   (third user-details)))
                 (redirect "/"))
                (t "Bad password"))
          "Non-existent user"))))
