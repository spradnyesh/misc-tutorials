(defun range (n1 n2)
  (let ((lst nil))
    (dotimes (i (1+ (- n2 n1)) lst)
      (setf lst (append lst (list (+ n1 i)))))))

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


(defun lotto-select (k n)
  (rnd-select (range 1 n) (- n k)))
