(defun a+fn (a)
        (lambda (n) (+ n a)))
(setf 2+ (a+fn 2))
(setf 3+ (a+fn 3))

(list (funcall 2+ 3) (funcall 3+ 4))
