(defun rev (lst)
  (if (null lst)
      nil
      (append (rev (rest lst)) (list (first lst)))))
