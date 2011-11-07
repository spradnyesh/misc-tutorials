(defun lsort (lst)
  (sort lst #'(lambda (a b)
                (when (< (length a) (length b)) t))))
