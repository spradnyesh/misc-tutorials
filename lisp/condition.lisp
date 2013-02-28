;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; http://chaitanyagupta.com/lisp/restarts.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-url (string)
  "The URL of the page; should start with http:// or https://."
  (unless (cl-ppcre:scan "^https?://" string)
    (csv-error "URL invalid." :value string)))


(defun validate-rating (string)
  "String should contain an integer between 1 and 5, inclusive."
  (let ((rating (parse-integer string :junk-allowed t)))
    (unless (and (integerp rating) (<= 1 rating 5))
      (csv-error "Rating not an integer in range." :value string))))


(defun validate-visitors (string)
  "The number of visitors to the page; string should contain an
integer more than or equal to zero."
  (let ((visitors (parse-integer string :junk-allowed t)))
    (unless (and (integerp visitors) (>= visitors 0))
      (csv-error "Number of visitors invalid." :value string))))


(defun validate-date (string)
  "The published date of the URL. Should be in yyyy-mm-dd format."
  (let ((split (cl-ppcre:split "-" string)))
    (flet ((!valid-number-of-digits-p (string n)   ; See note 3
             (and (every #'digit-char-p string)
                  (= (length string) n))))
      (unless (and (!valid-number-of-digits-p (first split) 4)
                   (!valid-number-of-digits-p (second split) 2)
                   (!valid-number-of-digits-p (third split) 2))
        (csv-error "Published date not in valid format." :value string)))))


(define-condition csv-error (error)
  ((message
    :initarg :message
    :accessor csv-error-message
    :initform nil
    :documentation "Text message indicating what went wrong with the validation.")
   (value
    :initarg :value
    :accessor csv-error-value
    :initform nil
    :documentation "The value of the field for which the error is signalled.")
   (line-number
    :initarg :line-number
    :accessor csv-error-line-number
    :initform nil
    :documentation "The line number of the row in for the error was signalled.")))


;; Do something more useful than the default printer behaviour
(defmethod print-object ((object csv-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[L~A ~]~S~@[: ~S~]"
            (csv-error-line-number object)
            (csv-error-message object)
            (csv-error-value object))))


;; We use this function to signal our validation error
(defun csv-error (message &key value line-number)
  (error 'csv-error
         :message message
         :value value
         :line-number line-number))


(defun parse-csv-file (file)
  (with-open-file (f file :direction :input)
    (loop
       for line = (read-line f nil)
       while line
       collect (cl-ppcre:split "," line))))


;; Takes a header name and a string value as arguments; checks the
;; validity of the value by calling the appropriate validator function
(defun validate-field (header value)
  (flet ((!header-matches (string)
           (string-equal header string)))
    (cond
      ((!header-matches "url") (validate-url value))
      ((!header-matches "rating") (validate-rating value))
      ((!header-matches "visitors") (validate-visitors value))
      ((!header-matches "date") (validate-date value))
      (t (csv-error "Invalid header." :value header)))))


#|(defun validate-csv (file)
  (destructuring-bind (headers . rows)
      (parse-csv-file file)
    (loop
       for row in rows
       for line-number upfrom 2
       do
         (when (/= (length row) (length headers))
           (csv-error "Number of fields doesn't equal number of headers."
                      :line-number line-number))
         (handler-bind
             ;; Set the LINE-NUMBER slot of the signalled
             ;; csv-error. Note that since this clause returns normally,
             ;; the error doesn't stop here, it goes "up" the stack
             ((csv-error #'(lambda (c)
                             (setf (csv-error-line-number c) line-number))))
           (loop
              for header in headers
              for field in row
              do (validate-field header field))))))|#


#|(defun validate-csv (file)
  (destructuring-bind (headers . rows)
      (parse-csv-file file)
    (loop
       for row in rows
       for line-number upfrom 2
       do
       ;; If this restart is invoked, validation will continue on
       ;; the next row
       (with-simple-restart (continue-next-row "Continue validation on next row.")
         (when (/= (length row) (length headers))
           (csv-error "Number of fields doesn't equal number of headers."
                      :line-number line-number))
         (loop
            for header in headers
            for field in row
            do
            (handler-bind
                ((csv-error #'(lambda (c)
                                (setf (csv-error-line-number c) line-number))))
              ;; If this restart is invoked, validation will continue
              ;; on the next field in the row
              (with-simple-restart (continue-next-field "Continue validation on next field.")
                (validate-field header field))))))))|#


(defun validate-csv (file)
  (restart-case (validate-csv-aux file)
    (retry-file ()
      :report (lambda (stream)
                (format stream "Retry validating the file ~A." file))
      :test (lambda (c) (typep c 'csv-error))
      (validate-csv file))))


(defun validate-csv-aux (file)
  (destructuring-bind (headers . rows)
      (parse-csv-file file)
    (loop
       for row in rows
       for line-number upfrom 2
       do
       (with-simple-restart (continue-next-row "Continue validation on next row.")
         (when (/= (length row) (length headers))
           (csv-error "Number of fields doesn't equal number of headers."
                      :line-number line-number))
         (handler-bind
                ((csv-error #'(lambda (c)
                                (setf (csv-error-line-number c) line-number))))
           (loop
            for header in headers
            for field in row
            do
              (with-simple-restart (continue-next-field "Continue validation on next field.")
                (validate-field header field))))))))


(defun list-csv-errors (file)
  (let ((result nil))
    (handler-bind
        ((csv-error #'(lambda (c)
                        (let ((restart (or (find-restart 'continue-next-field)
                                           (find-restart 'continue-next-row))))
                          (when restart
                            (push c result)
                            (invoke-restart restart))))))

      (validate-csv file))
    (nreverse result)))
