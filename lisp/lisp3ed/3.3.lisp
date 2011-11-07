(defun rotate-left (lst)
  (append (rest lst)
          (list (first lst))))
