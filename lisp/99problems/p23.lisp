(defun remove-at (lst n)
  (let ((l-pre nil))
    (dotimes (i (1- n))
      (setf l-pre (append l-pre (list (first lst))))
      (setf lst (rest lst)))
    (append l-pre (rest lst))))
(defun rnd-select (lst n)
  (when (<= n (length lst))
    (dotimes (i n lst)
      (setf lst (remove-at lst (random (1+ (length lst))))))))
