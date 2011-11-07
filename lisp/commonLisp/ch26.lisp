(defun fibo-gen (a b)
  (check-type a integer)
  (check-type b integer)
  #'(lambda ()
      (setf c a a b b (+ c a))
      c))

(setf my-fibo (fibo-gen 1 1))

(list (funcall my-fibo) (funcall my-fibo) (funcall my-fibo) (funcall my-fibo) (funcall my-fibo) (funcall my-fibo))


(defvar *stack*)
(setf *stack* '())
(defun my-push (e)
  (setf *stack* (cons e *stack*))
  *stack*)
(defun my-top ()
  (first *stack*))
(defun my-pop ()
  (let ((a (first *stack*)))
    (setf *stack* (rest *stack*))
    a))
