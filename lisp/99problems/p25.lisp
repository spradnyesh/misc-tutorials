(defun remove-at (lst n)
  (let ((l-pre nil))
    (dotimes (i (1- n))
      (setf l-pre (append l-pre (list (first lst))))
      (setf lst (rest lst)))
    (values (first lst) (append l-pre (rest lst)))))

(defun rnd-permu (lst)
  (let ((new-lst nil))
    (dotimes (i (length lst) new-lst)
      (multiple-value-bind (ele rst-lst)
          (remove-at lst (random (1+ (length lst))))
        (setf lst rst-lst)
        (setf new-lst (cons ele new-lst))))))
