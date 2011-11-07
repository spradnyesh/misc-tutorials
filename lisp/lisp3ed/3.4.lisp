(defun rotate-right (lst)
  (cons (first (last lst))
        (butlast lst)))
