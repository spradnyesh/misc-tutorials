(defun len-1 (lst)
  (if (null lst)
      0
      (+ 1 (len-1 (rest lst)))))
(defun len-2 (lst &optional (l 0))
  (if (null lst)
      l
      (len-2 (rest lst) (incf l))))
