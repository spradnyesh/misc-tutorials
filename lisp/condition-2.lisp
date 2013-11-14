;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; http://psg.com/~dlamkins/sl/chapter23.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition whats-wrong (error)
                    ((what :initarg :what :initform "something" :reader what))
    (:report (lambda (condition stream)
               (format stream "Foo! ~@(~A~) is wrong."
                       (what condition))))
    (:documentation "Tell the user that something is wrong."))

(define-condition whats-wrong-and-why (whats-wrong)
                    ((why :initarg :why :initform "no clue" :reader why))
    (:report (lambda (condition stream)
               (format stream "Uh oh! ~@(~A~) is wrong. Why? ~@(~A~)."
                       (what condition)
                       (why condition)))))

(define-condition whats-wrong-is-unfathomable (whats-wrong-and-why)
                    ()
    (:report (lambda (condition stream)
               (format stream "Gack! ~@(~A~) is wrong for some inexplicable reason."
                       (what condition)))))

#|(defun expect-type (object type default-value)
    (if (typep object type)
      object
      (progn
        (cerror "Substitute the default value ~2*~S."
                "~S is not of the expected type ~S."
                object type default-value)
        default-value)))|#

(define-condition expect-type-error (error)
  ((object :initarg :object :reader object)
   (my-type :initarg :my-type :reader my-type))
  (:report (lambda (condition stream)
             (format stream "~S is not of the expected type ~S."
                     (object condition)
                     (my-type condition)))))

(defun expect-type (object type default-value)
  (if (typep object type)
      object
      (progn
        (cerror "Substitute the default value ~5*~S."
                'expect-type-error
                :object object
                :my-type type
                :ignore default-value
                :allow-other-keys t)
        default-value)))

(defun my-divide (numerator denominator)
    (assert (not (zerop denominator)) (numerator denominator)
            "You can't divide ~D by ~D." numerator denominator)
    (/ numerator denominator))

(define-condition high-disk-utilization ()
                    ((disk-name :initarg :disk-name :reader disk-name)
                     (current :initarg :current :reader current-utilization)
                     (threshold :initarg :threshold :reader threshold))
    (:report (lambda (condition stream)
               (format stream "Disk ~A is ~D% full; threshold is ~D%."
                       (disk-name condition)
                       (current-utilization condition)
                       (threshold condition)))))

(defun get-disk-utilization (disk-name)
  (declare (ignore disk-name))
    ;; for this example, we'll just return a fixed value
    93)

(defun check-disk-utilization (name threshold)
    (let ((utilization (get-disk-utilization name)))
      (when (>= utilization threshold)
        (signal 'high-disk-utilization
                :disk-name name
                :current utilization
                :threshold threshold))))

(defun log-to-disk (record name)
  (handler-bind ((high-disk-utilization
                  #'(lambda (c)
                      (when (y-or-n-p "~&~A Panic?" c)
                        (return-from log-to-disk nil)))))
    (check-disk-utilization name 90)
    (print record))
  t)

(define-condition device-unresponsive ()
  ((device :initarg :device :reader device))
  (:report (lambda (condition stream)
             (format stream "Device ~A is unresponsive."
                     (device condition)))))

(defun send-query (device query)
    (format t "~&Sending ~S ~S~%" device query))

(defun accept-response (device)
  (declare (ignore device))
    ;; For the example, the device always fails.
    nil)

(defun reset-device (device)
    (format t "~&Resetting ~S~%" device))

(defun query-device (device)
  (restart-bind ((nil #'(lambda () (reset-device device))
                   :report-function
                   #'(lambda (stream)
                       (format stream "Reset device.")))
                 (nil #'(lambda ()
                          (format t "~&New device: ")
                          (finish-output)
                          (setq device (read)))
                   :report-function
                   #'(lambda (stream)
                       (format stream "Try a different device.")))
                 (nil #'(lambda ()
                          (return-from query-device :gave-up))
                   :report-function
                   #'(lambda (stream)
                       (format stream "Give up."))))
    (loop
       (send-query device 'query)
       (let ((answer (accept-response device)))
         (if answer
             (return answer)
             (cerror "Try again."
                     'device-unresponsive :device device))))))
