(defun range (n1 n2)
  (let ((lst nil))
    (dotimes (i (1+ (- n2 n1)) lst)
      (setf lst (append lst (list (+ n1 i)))))))
